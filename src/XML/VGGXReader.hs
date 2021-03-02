{-# LANGUAGE Arrows                    #-}

module XML.VGGXReader
( readVGGX
) where

import           Data.Tree.NTree.TypeDefs
import           Text.XML.HXT.Core
import           XML.Utilities
import           XML.XMLUtilities
import           Control.Monad

import qualified Control.Exception as E

import qualified Data.Tree as Tree
import qualified Data.Map as M
import           Data.Int
import           Data.Maybe
import qualified Data.Either as Either

import qualified Data.Graphs as G

import           GUI.Data.DiaGraph
import           GUI.Data.SaveInfo
import           GUI.Data.GraphicalInfo
import           GUI.Data.GraphState
import           GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as Info
import           GUI.Data.Nac (MergeMapping)


readVGGX :: String -> IO (Maybe (Tree.Forest SaveInfo))
readVGGX fileName = do
    -- read grammar
    typeGraph <- readTypeGraph fileName
    case typeGraph of
        Just tg -> do
            hg <- readHostGraph fileName
            rules <- readRules fileName
            return $ Just [tg,hg,rules]
        Nothing -> do
            error "Critical Error: failed to load TypeGraph -> parsing of the grammar failed"
            return Nothing


readTypeGraph :: String -> IO (Maybe (Tree.Tree SaveInfo))
readTypeGraph fileName = do
    typeGraphWithLogs <- runX $ parseXML fileName >>> parseTypeGraph
    case typeGraphWithLogs of
        [] -> return $ Nothing
        (logs,tg):_ -> do
            forM_ logs putStrLn
            return $ Just (Tree.Node tg [])

readHostGraph :: String -> IO (Tree.Tree SaveInfo)
readHostGraph fileName = do
    hostGraphWithLogs <- runX $ parseXML fileName >>> parseHostGraph
    case hostGraphWithLogs of
        [] -> return (Tree.Node (HostGraph 1 "initialGraph" (emptyState)) [])
        (logs,hg):_ -> do
            forM_ logs putStrLn
            return (Tree.Node hg [])

readRules :: String -> IO (Tree.Tree SaveInfo)
readRules fileName = do
    rulesOrLogs <- runX $ parseXML fileName >>> atTag "Rules" >>> parseRule
    let rules = map snd $ Either.rights rulesOrLogs
        criticalLogs = Either.lefts rulesOrLogs
        lessCriticalLogs = concat $ map fst $ Either.rights rulesOrLogs
    forM_ (criticalLogs ++ lessCriticalLogs) putStrLn
    return $ Tree.Node (Topic "Rules") rules

parseTypeGraph :: ArrowXml cat => cat (NTree XNode) ([String],SaveInfo)
parseTypeGraph = atTag "TypeGraph" >>>
    proc tg -> do
        name <- getAttrValue "name" -< tg
        idStr <- getAttrValue "id" -< tg
        (diagramLogs,(graph,layouts)) <- parseDiagram -< tg
        let gId = fromMaybe 0 $ parseId idStr fromIntegral :: Int32
            logs = if null diagramLogs
                    then []
                    else ("typeGraph " ++ idStr ++ " (" ++ name ++ ")"):diagramLogs
        returnA -< (diagramLogs, TypeGraph gId name (emptyState {stateGetGraph = graph, stateGetGI = layouts}))

parseHostGraph :: ArrowXml cat => cat (NTree XNode) ([String],SaveInfo)
parseHostGraph = atTag "HostGraph" >>>
    proc hg -> do
        name <- getAttrValue "name" -< hg
        idStr <- getAttrValue "id" -< hg
        (diagramLogs,(graph,layouts)) <- parseDiagram -< hg
        let gId = fromMaybe 1 $ parseId idStr fromIntegral :: Int32
            logs = if null diagramLogs
                        then []
                        else ("HostGraph " ++ idStr ++ " (" ++ name ++ ")"):diagramLogs
        returnA -< (diagramLogs, HostGraph gId name (emptyState {stateGetGraph = graph, stateGetGI = layouts}))

parseRule :: ArrowXml cat => cat (NTree XNode) (Either String ([String],Tree.Tree SaveInfo))
parseRule = atTag "Rule" >>>
    proc rl -> do
        name <- getAttrValue "name" -< rl
        idStr <- getAttrValue "id" -< rl
        aStr <- getAttrValue "active" -< rl
        g <- (getChildren >>> isElem >>> hasName "Graph") -< rl
        (diagramLogs,(graph,layouts)) <- parseDiagram -< g
        nacsOrLogs <- listA parseNac -< rl
        let gId = parseId idStr fromIntegral :: Maybe Int32
            ruleState = emptyState {stateGetGraph = graph, stateGetGI = layouts}
            active = parseBool aStr
            nacs = map snd $ Either.rights nacsOrLogs
            nacLogs = filter (not . null) $ Either.lefts nacsOrLogs ++ (concat $ map fst $ Either.rights nacsOrLogs)
            nacForest = map (\nac -> Tree.Node nac []) nacs
            struct = case gId of
                        Just i -> let logs = if null diagramLogs && null nacLogs
                                                then []
                                                else ("Rule " ++ idStr ++ " (" ++ name ++ ")"):(map (\str -> '-':str) $ diagramLogs ++ nacLogs)
                                  in Right (logs ,Tree.Node (RuleGraph i name ruleState active) nacForest)
                        Nothing -> Left $ "Failed to load Rule: Couldn't parse Rule ID. (id = " ++ idStr ++ ", name = " ++ name ++ ")."
        returnA -< struct

parseNac :: ArrowXml cat => cat (NTree XNode) (Either String ([String], SaveInfo))
parseNac = atTag "NAC" >>>
    proc nac -> do
        idStr <- getAttrValue "id" -< nac
        name <- getAttrValue "name" -< nac
        (diagramLogs,(graph,layouts)) <- parseDiagram -< nac
        mergeMap <- parseMergeMapping -< nac
        let gId = parseId idStr fromIntegral :: Maybe Int32
            struct = case gId of
                        Just i -> let logs = if null diagramLogs
                                                then []
                                                else ("NAC " ++ idStr ++ " (" ++ name ++ ")"):(map (\str -> '-':str) diagramLogs)
                                  in Right (logs, NacGraph i name ((graph,layouts), mergeMap))
                        Nothing ->  Left $ "Failed to load NAC: Couldn't parse NAC ID. (id = " ++ idStr ++ ", name = " ++ name ++ ")."
        returnA -< struct


parseMergeMapping :: ArrowXml cat => cat (NTree XNode) MergeMapping
parseMergeMapping = atTag "MergeMapping" >>>
    proc mergeMap -> do
        mNodes <- listA parseNodeMapping -< mergeMap
        mEdges <- listA parseEdgeMapping -< mergeMap
        let nodes = map fromJust . filter isJust $ mNodes
            edges = map fromJust . filter isJust $ mEdges
        returnA -< (M.fromList nodes, M.fromList edges)

parseNodeMapping :: ArrowXml cat => cat (NTree XNode) (Maybe (G.NodeId, G.NodeId))
parseNodeMapping = atTag "NodeMapping" >>>
    proc nMap -> do
        src <- getAttrValue "src" -< nMap
        tgt <- getAttrValue "tgt" -< nMap
        let srcId = parseId src G.NodeId
            tgtId = parseId tgt G.NodeId
        returnA -< (\x y -> (x,y)) <$> srcId <*> tgtId

parseEdgeMapping :: ArrowXml cat => cat (NTree XNode) (Maybe (G.EdgeId, G.EdgeId))
parseEdgeMapping = atTag "EdgeMapping" >>>
    proc eMap -> do
        src <- getAttrValue "src" -< eMap
        tgt <- getAttrValue "tgt" -< eMap
        let srcId = parseId src G.EdgeId
            tgtId = parseId tgt G.EdgeId
        returnA -< (\x y -> (x,y)) <$> srcId <*> tgtId

parseDiagram :: ArrowXml cat => cat (NTree XNode) ([String], (G.Graph Info Info, GraphicalInfo))
parseDiagram = proc x -> do
    (graphLogs, graph) <- parseGraph -< x
    (layoutsLogs, layouts) <- parseLayouts -< x
    returnA -< (graphLogs ++ layoutsLogs, (graph,layouts))

parseGraph :: ArrowXml cat => cat (NTree XNode) ([String],G.Graph Info Info)
parseGraph = atTag "Graph" >>>
    proc g -> do
        nodesOrLogs <- listA parseNode -< g
        edgesOrLogs <- listA parseEdge -< g
        let nodesWithLogs = Either.rights nodesOrLogs
            edgesWithLogs = Either.rights edgesOrLogs
            (nodesLogs,nodes) = (concat $ map fst nodesWithLogs, map snd nodesWithLogs)
            (edgesLogs,edges) = (concat $ map fst edgesWithLogs, map snd edgesWithLogs)
            criticalLogs = Either.lefts nodesOrLogs ++ Either.lefts edgesOrLogs
            logs = filter (not . null) (criticalLogs ++ nodesLogs ++ edgesLogs)
            graph = G.fromNodesAndEdges nodes edges
        returnA -< (logs,graph)

parseNode :: ArrowXml cat => cat (NTree XNode) (Either String ([String], G.Node Info))
parseNode = atTag "Node" >>>
    proc n -> do
        idStr <- getAttrValue "id" -< n
        let nid = parseId idStr G.NodeId
        (infoLog,info) <- parseInfo -< n
        returnA -< case nid of
                    Just i -> Right (infoLog, G.Node i info)
                    Nothing -> Left $ "Failed to load Node. Could not parse Node ID"

parseEdge :: ArrowXml cat => cat (NTree XNode) (Either String ([String], (G.Edge Info)))
parseEdge = atTag "Edge" >>>
    proc e -> do
        idStr <- getAttrValue "id" -< e
        src <- getAttrValue "source" -< e
        tgt <- getAttrValue "target" -< e
        let eid = parseId idStr G.EdgeId
            srcId = parseId src G.NodeId
            tgtId = parseId tgt G.NodeId
        (infoLog, info) <- parseInfo -< e
        returnA -< case (eid,srcId,tgtId) of
                        (Just e, Just s, Just t) -> Right (infoLog, G.Edge e s t info)
                        _ ->  Left $ "Failed to load Edge. Could not parse the ID of the Edge or of one of it's connected Nodes."

parseInfo :: ArrowXml cat => cat (NTree XNode) ([String],Info)
parseInfo = atTag "Info" >>>
    proc i -> do
        locked <- getAttrValue "locked" -< i
        itype <- getAttrValue "type" -< i
        operation <- getAttrValue "operation" -< i
        (log,label) <- parseInfoLabel -< i
        let info = Info.empty
                    { infoLocked = parseBool locked
                    , infoOperation = parseInfoOperation operation
                    , infoType = itype
                    , infoLabel = label
                    }
        returnA -< (log,info)

-- parse a label
parseInfoLabel :: ArrowXml cat => cat (NTree XNode) ([String],InfoLabel)
parseInfoLabel = atTag "Label" >>>
    proc l -> do
        isGroup <- getAttrValue "group" -< l
        let isGroup' = parseBool isGroup

        text <- getAttrValue "text" -< l
        elementsOrLogs <- listA parseLabelElements -< l
        let (logs,elements) = (filter (not . null) $ Either.lefts elementsOrLogs, Either.rights elementsOrLogs)
            labelWithLog = if isGroup'
                            then (logs, LabelGroup elements)
                            else ([], Label text)
        returnA -< labelWithLog

-- parse elements of a LabelGroup
parseLabelElements :: ArrowXml cat => cat (NTree XNode) (Either String (Int,String))
parseLabelElements = atTag "LabelElement" >>>
    proc e -> do
        text <- getAttrValue "text" -< e
        idStr <- getAttrValue "id" -< e
        let struct = case parseId idStr id of
                            Nothing -> Left $ "Failed to load LabelElement. Could not parse Element ID "
                            Just i -> Right (i, text)
        returnA -< struct

-- parse layouts, returning a list of possible error logs and the layouts for the graph (GraphicalInfo)
parseLayouts :: ArrowXml cat => cat (NTree XNode) ([String],GraphicalInfo)
parseLayouts = atTag "Graph" >>>
    proc g -> do
        nlayoutsOrLogs <- listA parseNodeLayout -< g
        elayoutsOrLogs <- listA parseEdgeLayout -< g
        let nlayouts = Either.rights nlayoutsOrLogs
            elayouts = Either.rights elayoutsOrLogs
            nlogs = Either.lefts nlayoutsOrLogs
            elogs = Either.lefts elayoutsOrLogs
        returnA -< (nlogs++elogs,(M.fromList nlayouts, M.fromList elayouts))

-- parse a node layout (NodeGI), returning a log string in case of error
-- values are set to defaults in case their parse fails
parseNodeLayout :: ArrowXml cat => cat (NTree XNode) (Either String (Int, NodeGI))
parseNodeLayout = atTag "Node" >>>
    proc n -> do
        idStr <- getAttrValue "id" -< n
        l <- atTag "NodeLayout" -< n
        posStr <- getAttrValue "position" -< l
        fColorStr <- getAttrValue "fillColor" -< l
        lColorStr <- getAttrValue "lineColor" -< l
        dimsStr    <- getAttrValue "dimension" -< l
        shapeStr  <- getAttrValue "shape" -< l

        let struct = case parseId idStr id of
                        Nothing -> Left "Failed to load NodeLayout. Could not parse Node ID."
                        Just i ->
                            let pos = fromMaybe (0,0) $ parseDoublePair posStr
                                fColor = fromMaybe (1,1,1) $ parseDoubleTriple fColorStr
                                lColor = fromMaybe (0,0,0) $ parseDoubleTriple lColorStr
                                dims = fromMaybe (20,20) $ parseDoublePair dimsStr
                                shape = parseNodeShape shapeStr
                                in Right (i, NodeGI pos fColor lColor dims shape)
        returnA -< struct

-- parse a edge layout (EdgeGI), returning a log string in case of error
parseEdgeLayout :: ArrowXml cat => cat (NTree XNode) (Either String (Int, EdgeGI))
parseEdgeLayout = atTag "Edge" >>>
    proc e -> do
        idStr <- getAttrValue "id" -< e
        l <- atTag "EdgeLayout" -< e
        posStr <- getAttrValue "position" -< l
        colStr <- getAttrValue "color" -< l
        styleStr <- getAttrValue "style" -< l
        let struct = case parseId idStr id of
                        Nothing -> Left "Failed to load EdgeLayout. Could not parse Edge ID."
                        Just i ->
                            let pos = fromMaybe (0,0) $ parseDoublePair posStr
                                col = fromMaybe (0,0,0) $ parseDoubleTriple colStr
                                style = parseEdgeStyle styleStr
                            in Right (i, EdgeGI pos col style)
        returnA -< struct


-- auxiliar parsing functions to simple types and structures -- parsing can fail returning nothing
parseId :: Num a => String -> (Int -> a) -> Maybe a
parseId str f = case reads str :: [(Int,String)] of
                [(num,_)] -> Just $ f num
                _ -> Nothing

parseDoublePair :: String -> Maybe (Double,Double)
parseDoublePair str = case reads str :: [((Double,Double),String)] of
                        [((a,b),_)] -> Just (a,b)
                        _ -> Nothing

parseDoubleTriple :: String -> Maybe (Double,Double,Double)
parseDoubleTriple str = case reads str :: [((Double,Double,Double),String)] of
                        [((a,b,c),_)] -> Just (a,b,c)
                        _ -> Nothing

-- parse functions for simple types -- upon failure returns a default value
parseBool :: String -> Bool
parseBool str = case reads str :: [(Bool,String)] of
                    [(b,_)] -> b
                    _ -> False

parseInfoOperation :: String -> InfoOperation
parseInfoOperation str = case reads str :: [(InfoOperation,String)] of
                            [(op,_)] -> op
                            _ -> Preserve

parseEdgeStyle :: String -> EdgeStyle
parseEdgeStyle str = case reads str :: [(EdgeStyle,String)] of
                [(style,_)] -> style
                _ -> ENormal

parseNodeShape :: String -> NodeShape
parseNodeShape str = case reads str :: [(NodeShape,String)] of
                [(shape,_)] -> shape
                _ -> NCircle
