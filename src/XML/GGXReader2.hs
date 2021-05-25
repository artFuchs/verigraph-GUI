{-# LANGUAGE Arrows                    #-}

module XML.GGXReader2
( readGGX
, readTypes
) where


import           Data.Tree.NTree.TypeDefs
import           Text.XML.HXT.Core
import qualified XML.Utilities as Utils
import           XML.XMLUtilities
import           Control.Monad

import qualified Control.Exception as E

import qualified Data.Tree as Tree
import qualified Data.Map as M
import           Data.Int
import           Data.Maybe
import           Data.List.Split
import qualified Data.Either as Either

import qualified Data.Graphs as G

import           GUI.Data.DiaGraph
import           GUI.Data.SaveInfo
import           GUI.Data.GraphicalInfo
import           GUI.Data.GraphState
import           GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as Info
import           GUI.Data.Nac (MergeMapping)

-- ElementType = Map typeId (typeName typeLayout)
type ElementTypes = (M.Map String (String, NodeGI), M.Map String (String, EdgeGI))

readGGX :: String -> IO (Maybe (Tree.Forest SaveInfo))
readGGX fileName = do
  -- read types to get layouts
  elemTypes <- readTypes fileName
  typeGraph <- readTypeGraph fileName elemTypes
  case typeGraph of
    Just tg -> do
      hg <- readHostGraph fileName elemTypes >>= return . fromMaybe (HostGraph 1 "initialGraph" emptyState)
      rules <- readRules fileName elemTypes >>= return . map (\a -> Tree.Node a [])
      return $ Just [ Tree.Node tg []
                    , Tree.Node hg []
                    , Tree.Node (Topic "Rules") rules
                    ]
    _ -> return Nothing

readTypes :: String -> IO ElementTypes
readTypes fileName = do
  nodeTypes <- runX $ parseXML fileName >>> parseNodeType
  edgeTypes <- runX $ parseXML fileName >>> parseEdgeType
  return (M.fromList nodeTypes, M.fromList edgeTypes)

readTypeGraph :: String -> ElementTypes -> IO (Maybe SaveInfo)
readTypeGraph fileName types = do
  tgs <- runX $ parseXML fileName >>> parseTypeGraph types
  case tgs of
    tg:_ -> return $ Just tg
    _ -> return Nothing

readHostGraph :: String -> ElementTypes -> IO (Maybe SaveInfo)
readHostGraph fileName types = do
  gs <- runX $ parseXML fileName >>> parseGraph types
  let hgs = filter (\(k,_,_) -> k=="HOST") gs
  case hgs of
    (_,i,st):_ -> return $ Just (HostGraph (fromMaybe 1 $ parseId (Utils.clearId i) fromIntegral) "InitialGraph" st)
    _ -> return Nothing

readRules :: String -> ElementTypes -> IO [SaveInfo]
readRules fileName types = do
  elemIds <- runX $ parseXML fileName >>> getElemIds -- [([String],[String])]
  let elementIds = foldr (\(ns,es) (ns',es') ->  (ns++ns',es++es')) ([],[]) elemIds
  rules <- runX $ parseXML fileName >>> parseRule types elementIds
  let msgs = unlines $ map fst rules
  putStrLn msgs
  return $ catMaybes (map snd rules)


parseNodeType :: ArrowXml cat => cat (NTree XNode) (String,(String,NodeGI))
parseNodeType = atTag "Types" >>> atTag "NodeType" >>>
  proc nt -> do
    idStr <- getAttrValue "ID" -< nt
    str <- getAttrValue "name" -< nt
    let (name,gi) = parseNodeTypeName str
    returnA -< (idStr,(name,gi))

parseNodeTypeName :: String -> (String,NodeGI)
parseNodeTypeName [] = ([],newNodeGI)
parseNodeTypeName l = (name, gi)
  where
    name:rest = wordsBy (\c -> c == '%' || c == ':')  l
    possibleForms = catMaybes
                    $ map (\str -> case str of
                            "CIRCLE" -> Just NCircle
                            "RECT" -> Just NRect
                            "ROUNDRECT" -> Just NSquare
                            "OVAL" -> Just NCircle
                            _ -> Nothing)
                      rest
    possibleColors = catMaybes
                    $ map (\str ->
                            let dividedStr = wordsBy (\c -> c == '[' || c == ']' || c == ',') str
                                colorStrs = catMaybes
                                          $ map (\cstr -> case cstr of
                                                  'r':'=':r -> Just r
                                                  'g':'=':g -> Just g
                                                  'b':'=':b -> Just b
                                                  _ -> Nothing)
                                            dividedStr
                            in if length colorStrs == 3
                                then Just colorStrs
                                else Nothing
                          )
                      rest
    form = case possibleForms of
            [] -> NCircle
            f:_ -> f
    color = case possibleColors of
            [] -> (1.0,1.0,1.0)
            [r,g,b]:_ -> case (parseId r ( (/255) . fromIntegral ), parseId g ( (/255) . fromIntegral ), parseId b ( (/255) . fromIntegral )) of
                          (Just r', Just g', Just b') -> if (r',g',b') == (0.0,0.0,0.0) then (1.0,1.0,1.0) else (r',g',b')
                          _ -> (1.0,1.0,1.0)
    gi = newNodeGI {shape = form, fillColor = color}

parseEdgeType :: ArrowXml cat => cat (NTree XNode) (String,(String,EdgeGI))
parseEdgeType = atTag "Types" >>> atTag "EdgeType" >>>
  proc et -> do
    idStr <- getAttrValue "ID" -< et
    str <- getAttrValue "name" -< et
    let (name,gi) = parseEdgeTypeName str
    returnA -< (idStr,(name,gi))

parseEdgeTypeName :: String -> (String,EdgeGI)
parseEdgeTypeName [] = ([],newEdgeGI)
parseEdgeTypeName l = (name, gi)
  where
    name:rest = wordsBy (\c -> c == '%' || c == ':')  l
    possibleForms = catMaybes
                    $ map (\str -> case str of
                            "SOLID_LINE" -> Just ENormal
                            "DOT_LINE" -> Just EPointed
                            "DASH_LINE" -> Just ESlashed
                            _ -> Nothing)
                      rest
    possibleColors = catMaybes
                    $ map (\str ->
                            let dividedStr = wordsBy (\c -> c == '[' || c == ']' || c == ',') str
                                colorStrs = catMaybes
                                          $ map (\cstr -> case cstr of
                                                  'r':'=':r -> Just r
                                                  'g':'=':g -> Just g
                                                  'b':'=':b -> Just b
                                                  _ -> Nothing)
                                            dividedStr
                            in if length colorStrs == 3
                                then Just colorStrs
                                else Nothing
                          )
                      rest
    form = case possibleForms of
            [] -> ENormal
            f:_ -> f
    lcolor = case possibleColors of
            [] -> (1.0,1.0,1.0)
            [r,g,b]:_ -> case (parseId r ( (/255) . fromIntegral ), parseId g ( (/255) . fromIntegral ), parseId b ( (/255) . fromIntegral )) of
                          (Just r', Just g', Just b') -> (r',g',b')
                          _ -> (1.0,1.0,1.0)
    gi = newEdgeGI {style = form, color = lcolor}


parseTypeGraph :: ArrowXml cat => ElementTypes -> cat (NTree XNode) SaveInfo
parseTypeGraph (ntypes,etypes) = atTag "Types" >>> atTag "Graph" >>>
  proc tg -> do
    nodes <- listA $ parseTypeNode ntypes -< tg
    edges <- listA $ parseTypeEdge etypes -< tg
    let g = G.fromNodesAndEdges (map fst $ catMaybes nodes) [] --(map fst $ catMaybes edges)
    let nlyts = M.fromList $ map (\(n,l) -> (fromIntegral $ G.nodeId n, l)) $ catMaybes nodes
    let st = stateSetGraph g . stateSetGI (nlyts,M.empty) $ emptyState
    let st' = foldr
              (\(e,l) st -> createEdge st (Just $ G.edgeId e) (G.sourceId e) (G.targetId e) (G.edgeInfo e) False (style l) (color l))
              st
              (catMaybes edges)
    returnA -< (TypeGraph 0 "TypeGraph" st')

parseTypeNode :: ArrowXml cat => M.Map String (String, NodeGI) -> cat (NTree XNode) (Maybe (G.Node Info, NodeGI))
parseTypeNode ntypes = atTag "Node" >>>
  proc nd -> do
    idStr <- getAttrValue "ID" -< nd
    t <- getAttrValue "type" -< nd
    npos <- parseNodePos -< nd
    let mnid = parseId (Utils.clearId idStr) G.NodeId
        mt = M.lookup t ntypes
        mnode = case (mnid,mt) of
                  (Just nid, Just (lbl,lyt)) -> Just (G.Node nid (Info.empty {infoLabel = Label lbl}), lyt {position = npos})
                  _ -> Nothing
    returnA -< mnode

parseNodePos :: ArrowXml cat => cat (NTree XNode) (Double,Double)
parseNodePos = atTag "NodeLayout" >>>
  proc nl -> do
    xStr <- getAttrValue "X" -< nl
    yStr <- getAttrValue "Y" -< nl
    let mx = parseId xStr fromIntegral :: Maybe Double
    let my = parseId yStr fromIntegral :: Maybe Double
    returnA -< (fromMaybe 0.0 mx, fromMaybe 0.0 my)

parseTypeEdge :: ArrowXml cat => M.Map String (String, EdgeGI) -> cat (NTree XNode) (Maybe (G.Edge Info, EdgeGI))
parseTypeEdge etypes = atTag "Edge" >>>
  proc edg -> do
    idStr <- getAttrValue "ID" -< edg
    srcStr <- getAttrValue "source" -< edg
    tgtStr <- getAttrValue "target" -< edg
    t <- getAttrValue "type" -< edg
    let meid = parseId (Utils.clearId idStr) G.EdgeId
        msrc = parseId (Utils.clearId srcStr) G.NodeId
        mtgt = parseId (Utils.clearId tgtStr) G.NodeId
        mt = M.lookup t etypes
        medge = case (meid,msrc,mtgt,mt) of
                  (Just eid, Just src, Just tgt, Just (lbl,lyt)) -> Just (G.Edge eid src tgt (Info.empty {infoLabel = Label lbl}), lyt)
                  _ -> Nothing
    returnA -< medge


parseRule :: ArrowXml cat => ElementTypes -> ([String],[String]) -> cat (NTree XNode) (String, Maybe SaveInfo)
parseRule (ntypes, etypes) elemIds = atTag "Rule" >>>
  proc rg -> do
    idStr <- getAttrValue "ID" -< rg
    name <- getAttrValue "name" -< rg
    -- get the left and right graphs
    mappings <- listA $ parseMorphism elemIds -< rg
    -- merge the graph states
    graphs <- listA $ parseGraph (ntypes, etypes) -< rg
    let (_,_,leftGst) = head $ filter (\(k,_,_) -> k == "LHS") graphs
        (_,_,rightGst) = head $ filter (\(k,_,_) -> k == "RHS") graphs
        (nodeM,edgeM) = head mappings
        preservedNodes = filter (\n -> G.nodeId n `elem` (M.keys nodeM)) (G.nodes (stateGetGraph leftGst))
        preservedEdges = filter (\e -> G.edgeId e `elem` (M.keys edgeM)) (G.edges (stateGetGraph leftGst))
        removedNodes =   filter (\n -> G.nodeId n `notElem` (M.keys nodeM)) (G.nodes (stateGetGraph leftGst))
        removedEdges =   filter (\e -> G.edgeId e `notElem` (M.keys edgeM)) (G.edges (stateGetGraph leftGst))
        addedNodes =     filter (\n -> G.nodeId n `notElem` (M.elems nodeM)) (G.nodes (stateGetGraph rightGst))
        addedEdges =     filter (\e -> G.edgeId e `notElem` (M.elems edgeM)) (G.edges (stateGetGraph rightGst))
        removedNodes' = map (\n -> n { G.nodeInfo = (G.nodeInfo n) {infoOperation = Delete}}) removedNodes
        removedEdges' = map (\e -> e { G.edgeInfo = (G.edgeInfo e) {infoOperation = Delete}}) removedEdges
        addedNodes' = map (\n -> n { G.nodeInfo = (G.nodeInfo n) {infoOperation = Create}}) addedNodes
        addedEdges' = map (\e -> e { G.edgeInfo = (G.edgeInfo e) {infoOperation = Create}}) addedEdges
        ruleG = G.fromNodesAndEdges (preservedNodes ++ removedNodes' ++ addedNodes') (preservedEdges ++ removedEdges' ++ addedEdges')
        (leftNGI,leftEGI) = stateGetGI leftGst
        rightNGI = M.filterWithKey (\k _ -> G.NodeId k `elem` (map G.nodeId addedNodes')) (fst $ stateGetGI rightGst)
        rightEGI = M.filterWithKey (\k _ -> G.EdgeId k `elem` (map G.edgeId addedEdges')) (snd $ stateGetGI rightGst)
        ruleGI = (M.union leftNGI rightNGI, M.union leftEGI rightEGI)
        ruleSt = stateSetGI ruleGI . stateSetGraph ruleG $ emptyState
    -- create the saveInfo and return
    let rid = parseId (Utils.clearId idStr) fromIntegral
        sinfo = case rid of
                    Just i -> Just $ RuleGraph i name ruleSt True
                    Nothing -> Nothing
        msg = name ++ "\nruleG: " ++ (show ruleG) ++ "\nLHS: " ++ (show (stateGetGraph leftGst)) ++ "\nRHS: " ++ (show (stateGetGraph rightGst)) ++ "\nnode mapping:" ++ (show nodeM) ++ "\nedge mapping:" ++ (show edgeM)
    returnA -< (msg,sinfo)


getElemIds :: ArrowXml cat => cat (NTree XNode) ([String],[String])
getElemIds = atTag "Graph" >>>
  proc g -> do
    nodeStrs <- listA $ (atTag "Node" >>> getAttrValue "ID") -< g
    edgeStrs <- listA $ (atTag "Edge" >>> getAttrValue "ID") -< g
    returnA -< (nodeStrs,edgeStrs)

type Mapping = (M.Map G.NodeId G.NodeId, M.Map G.EdgeId G.EdgeId)
type NodeMapping = (G.NodeId, G.NodeId)
type EdgeMapping = (G.EdgeId, G.EdgeId)

parseMorphism :: ArrowXml cat => ([String],[String]) -> cat (NTree XNode) Mapping
parseMorphism (nodeIds,edgeIds) = atTag "Morphism" >>>
  proc m -> do
    mappings <- listA $ parseMapping (nodeIds, edgeIds) -< m
    let nodeMappings = M.fromList $ catMaybes $ map fst mappings
        edgeMappings = M.fromList $ catMaybes $ map snd mappings
    returnA -< (nodeMappings, edgeMappings)

parseMapping :: ArrowXml cat => ([String],[String]) -> cat (NTree XNode) (Maybe NodeMapping, Maybe EdgeMapping)
parseMapping (nodeIds, edgeIds) = atTag "Mapping" >>>
  proc m -> do
    srcStr <- getAttrValue "orig" -< m
    tgtStr <- getAttrValue "image" -< m
    let isNode = (srcStr `elem` nodeIds) && (tgtStr `elem` nodeIds)
        isEdge = (srcStr `elem` edgeIds) && (tgtStr `elem` edgeIds)
        srcNodeId = if isNode then parseId (Utils.clearId srcStr) G.NodeId else Nothing
        tgtNodeId = if isNode then parseId (Utils.clearId tgtStr) G.NodeId else Nothing
        srcEdgeId = if isEdge then parseId (Utils.clearId srcStr) G.EdgeId else Nothing
        tgtEdgeId = if isEdge then parseId (Utils.clearId tgtStr) G.EdgeId else Nothing
        nMapping = (\a b -> (a,b)) <$> srcNodeId <*> tgtNodeId
        eMapping = (\a b -> (a,b)) <$> srcEdgeId <*> tgtEdgeId
    returnA -< (nMapping,eMapping)


-- parse a graph together with the elements' layouts, generating a graphState
parseGraph :: ArrowXml cat => ElementTypes -> cat (NTree XNode) (String,String,GraphState)
parseGraph (ntypes,etypes) = atTag "Graph" >>>
  proc g -> do
    kindStr <- getAttrValue "kind" -< g
    idStr <- getAttrValue "ID" -< g
    -- parse elements
    nodes <- listA $ parseNode ntypes -< g
    edges <- listA $ parseEdge etypes -< g
    -- create a graph with only nodes and add edges one by one
    let g = G.fromNodesAndEdges (map fst $ catMaybes nodes) []
        nlyts = M.fromList $ map (\(n,l) -> (fromIntegral $ G.nodeId n, l)) $ catMaybes nodes
        st = stateSetGraph g . stateSetGI (nlyts,M.empty) $ emptyState
        st' = foldr
              (\(e,l) st -> createEdge st (Just $ G.edgeId e) (G.sourceId e) (G.targetId e) (G.edgeInfo e) False (style l) (color l) )
              st
              (catMaybes edges)
    returnA -< (kindStr, idStr, st')


parseNode :: ArrowXml cat => M.Map String (String, NodeGI) -> cat (NTree XNode) (Maybe (G.Node Info, NodeGI))
parseNode ntypes = atTag "Node" >>>
  proc nd -> do
    idStr <- getAttrValue "ID" -< nd
    t <- getAttrValue "type" -< nd
    npos <- parseNodePos -< nd
    let mnid = parseId (Utils.clearId idStr) G.NodeId
        mt = M.lookup t ntypes
        mnode = case (mnid,mt) of
                  (Just nid, Just (t,lyt)) -> Just (G.Node nid (Info.empty {infoLabel = Label (show . fromEnum $ nid), infoType = t}), lyt {position = npos})
                  _ -> Nothing
    returnA -< mnode

parseEdge :: ArrowXml cat => M.Map String (String, EdgeGI) -> cat (NTree XNode) (Maybe (G.Edge Info, EdgeGI))
parseEdge etypes = atTag "Edge" >>>
  proc edg -> do
    idStr <- getAttrValue "ID" -< edg
    srcStr <- getAttrValue "source" -< edg
    tgtStr <- getAttrValue "target" -< edg
    t <- getAttrValue "type" -< edg
    let meid = parseId (Utils.clearId idStr) G.EdgeId
        msrc = parseId (Utils.clearId srcStr) G.NodeId
        mtgt = parseId (Utils.clearId tgtStr) G.NodeId
        mt = M.lookup t etypes
        medge = case (meid,msrc,mtgt,mt) of
                  (Just eid, Just src, Just tgt, Just (t,lyt)) -> Just (G.Edge eid src tgt (Info.empty {infoLabel = Label (show . fromEnum $ eid), infoType = t}), lyt)
                  _ -> Nothing
    returnA -< medge

-- auxiliar parsing functions to simple types and structures -- parsing can fail returning nothing
parseId :: Num a => String -> (Int -> a) -> Maybe a
parseId str f = case reads str :: [(Int,String)] of
                [(num,_)] -> Just $ f num
                _ -> Nothing
