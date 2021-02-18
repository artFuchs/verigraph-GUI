{-# LANGUAGE Arrows                    #-}

module XML.VGGXReader
( readVGGX
) where

import           Data.Tree.NTree.TypeDefs
import           Text.XML.HXT.Core
import           XML.Utilities
import           XML.XMLUtilities

import qualified Control.Exception as E

import qualified Data.Tree as Tree
import qualified Data.Map as M
import           Data.Int
import           Data.Maybe

import qualified Data.Graphs as G

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
            hostGraph <- readHostGraph fileName
            rules <- readRules fileName
            let hg = fromMaybe (Tree.Node (HostGraph 1 "initialGraph" (emptyState)) []) hostGraph
            return $ Just [tg,hg,rules]
        Nothing -> return Nothing
    
    
readTypeGraph :: String -> IO (Maybe (Tree.Tree SaveInfo))
readTypeGraph fileName = do
    typeGraph <- runX $ parseXML fileName >>> parseTypeGraph
    case typeGraph of 
        [] -> return $ Nothing
        tg:_ -> return $ Just (Tree.Node tg [])

readHostGraph :: String -> IO (Maybe (Tree.Tree SaveInfo))
readHostGraph fileName = do
    hostGraph <- runX $ parseXML fileName >>> parseHostGraph
    case hostGraph of
        [] -> return Nothing 
        hg:_ -> return $ Just (Tree.Node hg [])

readRules :: String -> IO (Tree.Tree SaveInfo)
readRules fileName = do
    rules <- runX $ parseXML fileName >>> atTag "Rules" >>> parseRule
    return $ Tree.Node (Topic "Rules") rules

parseTypeGraph :: ArrowXml cat => cat (NTree XNode) SaveInfo
parseTypeGraph = atTag "TypeGraph" >>>
    proc tg -> do
        name <- getAttrValue "name" -< tg
        id <- getAttrValue "id" -< tg
        graph <- parseGraph -< tg
        layouts <- parseLayouts -< tg
        returnA -< TypeGraph (read id) name (emptyState {stateGetGraph = graph, stateGetGI = layouts})

parseHostGraph :: ArrowXml cat => cat (NTree XNode) SaveInfo
parseHostGraph = atTag "HostGraph" >>>
    proc hg -> do
        name <- getAttrValue "name" -< hg
        id <- getAttrValue "id" -< hg
        graph <- parseGraph -< hg
        layouts <- parseLayouts -< hg
        returnA -< HostGraph (read id) name (emptyState {stateGetGraph = graph, stateGetGI = layouts})

parseRule :: ArrowXml cat => cat (NTree XNode) (Tree.Tree SaveInfo)
parseRule = atTag "Rule" >>>
    proc rl -> do 
        name <- getAttrValue "name" -< rl
        id <- getAttrValue "id" -< rl
        active <- getAttrValue "active" -< rl
        g <- (getChildren >>> isElem >>> hasName "Graph") -< rl
        graph <- parseGraph -< g
        layouts <- parseLayouts -< g
        nacs <- listA parseNac -< rl
        let ruleState = emptyState {stateGetGraph = graph, stateGetGI = layouts}
            ruleSI = RuleGraph (read id) name ruleState (read active)
            nacForest = map (\nac -> Tree.Node nac []) nacs
        returnA -< Tree.Node ruleSI nacForest

parseNac :: ArrowXml cat => cat (NTree XNode) SaveInfo
parseNac = atTag "NAC" >>> 
    proc nac -> do
        id <- getAttrValue "id" -< nac
        name <- getAttrValue "name" -< nac 
        graph <- parseGraph -< nac 
        layouts <- parseLayouts -< nac 
        mergeMap <- parseMergeMapping -< nac 
        returnA -< NacGraph (read id) name ((graph,layouts), mergeMap)

parseMergeMapping :: ArrowXml cat => cat (NTree XNode) MergeMapping
parseMergeMapping = atTag "MergeMapping" >>>
    proc mMap -> do 
        nodes <- listA parseNodeMapping -< mMap
        edges <- listA parseEdgeMapping -< mMap
        returnA -< (M.fromList nodes, M.fromList edges)

parseNodeMapping :: ArrowXml cat => cat (NTree XNode) (G.NodeId, G.NodeId)
parseNodeMapping = atTag "NodeMapping" >>>
    proc nMap -> do
        src <- getAttrValue "src" -< nMap 
        tgt <- getAttrValue "tgt" -< nMap 
        returnA -< (G.NodeId (read src), G.NodeId (read tgt))

parseEdgeMapping :: ArrowXml cat => cat (NTree XNode) (G.EdgeId, G.EdgeId)
parseEdgeMapping = atTag "EdgeMapping" >>>
    proc eMap -> do
        src <- getAttrValue "src" -< eMap 
        tgt <- getAttrValue "tgt" -< eMap 
        returnA -< (G.EdgeId (read src), G.EdgeId (read tgt))

parseGraph :: ArrowXml cat => cat (NTree XNode) (G.Graph Info Info)
parseGraph = atTag "Graph" >>> 
    proc g -> do 
        nodes <- listA parseNode -< g
        edges <- listA parseEdge -< g
        let graph = G.fromNodesAndEdges nodes edges
        returnA -< graph

parseNode :: ArrowXml cat => cat (NTree XNode) (G.Node Info)
parseNode = atTag "Node" >>>
    proc n -> do
        id <- getAttrValue "id" -< n
        let nid = G.NodeId (read id)
        info <- parseInfo -< n
        returnA -< G.Node nid info

parseEdge :: ArrowXml cat => cat (NTree XNode) (G.Edge Info)
parseEdge = atTag "Edge" >>>
    proc e -> do 
        id <- getAttrValue "id" -< e
        src <- getAttrValue "source" -< e 
        tgt <- getAttrValue "target" -< e
        let eid = G.EdgeId (read id)
            srcId = G.NodeId (read src)
            tgtId = G.NodeId (read tgt)
        info <- parseInfo -< e
        returnA -< G.Edge eid srcId tgtId info 

parseInfo :: ArrowXml cat => cat (NTree XNode) Info
parseInfo = atTag "Info" >>>
    proc i -> do
        locked <- getAttrValue "locked" -< i
        itype <- getAttrValue "type" -< i
        operation <- getAttrValue "operation" -< i
        label <- parseInfoLabel -< i
        returnA -< Info.empty 
                        { infoLocked = (read locked)
                        , infoOperation = (read operation)
                        , infoType = itype
                        , infoLabel = label
                        }

parseInfoLabel :: ArrowXml cat => cat (NTree XNode) InfoLabel
parseInfoLabel = atTag "Label" >>>
    proc l -> do
        isGroup <- getAttrValue "group" -< l
        let isGroup' = case reads isGroup :: [(Bool, String)] of 
                            [(tval,_)] -> tval
                            _ -> False
        
        text <- getAttrValue "text" -< l
        elements <- listA parseLabelElements -< l
        returnA -< if isGroup' 
                    then LabelGroup elements
                    else Label text 


parseLabelElements :: ArrowXml cat => cat (NTree XNode) (Int,String)
parseLabelElements = atTag "LabelElement" >>>
    proc e -> do
        text <- getAttrValue "text" -< e 
        id <- getAttrValue "id" -< e 
        returnA -< (read id, text)

parseLayouts :: ArrowXml cat => cat (NTree XNode) GraphicalInfo
parseLayouts = atTag "Graph" >>>
    proc g -> do
        nlayouts <- listA parseNodeLayout -< g
        elayouts <- listA parseEdgeLayout -< g
        returnA -< (M.fromList nlayouts, M.fromList elayouts)

parseNodeLayout :: ArrowXml cat => cat (NTree XNode) (Int, NodeGI)
parseNodeLayout = atTag "Node" >>> 
    proc n -> do 
        id <- getAttrValue "id" -< n
        l <- atTag "NodeLayout" -< n
        pos <- getAttrValue "position" -< l
        fColor <- getAttrValue "fillColor" -< l 
        lColor <- getAttrValue "lineColor" -< l 
        dims    <- getAttrValue "dimension" -< l 
        shape  <- getAttrValue "shape" -< l
        let gi = NodeGI (read pos) (read fColor) (read lColor) (read dims) (read shape)
        returnA -< (read id, gi)

parseEdgeLayout :: ArrowXml cat => cat (NTree XNode) (Int, EdgeGI)
parseEdgeLayout = atTag "Edge" >>>
    proc e -> do
        id <- getAttrValue "id" -< e
        l <- atTag "EdgeLayout" -< e
        pos <- getAttrValue "position" -< l 
        col <- getAttrValue "color" -< l
        style <- getAttrValue "style" -< l 
        let gi = EdgeGI (read pos) (read col) (read style)
        returnA -< (read id, gi)

    