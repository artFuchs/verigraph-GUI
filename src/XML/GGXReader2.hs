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
      return $ Just [ Tree.Node tg []
                    , Tree.Node hg []
                    , Tree.Node (Topic "Rules") [ Tree.Node (RuleGraph 2 "rule 0" emptyState True) [] ]
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
  mhgs <- runX $ parseXML fileName >>> parseHostGraph types
  let hgs = catMaybes mhgs
  case hgs of
    hg:_ -> return $ Just hg
    _ -> return Nothing

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
              (\(e,l) st -> createEdge st (G.sourceId e) (G.targetId e) (G.edgeInfo e) False (style l) (color l))
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

parseHostGraph :: ArrowXml cat => ElementTypes -> cat (NTree XNode) (Maybe SaveInfo)
parseHostGraph (ntypes,etypes) = atTag "Graph" >>>
  proc hg -> do
    kindStr <- getAttrValue "kind" -< hg
    nodes <- listA $ parseNode ntypes -< hg
    edges <- listA $ parseEdge etypes -< hg
    let g = G.fromNodesAndEdges (map fst $ catMaybes nodes) []
    let nlyts = M.fromList $ map (\(n,l) -> (fromIntegral $ G.nodeId n, l)) $ catMaybes nodes
    let st = stateSetGraph g . stateSetGI (nlyts,M.empty) $ emptyState
    let st' = foldr
              (\(e,l) st -> createEdge st (G.sourceId e) (G.targetId e) (G.edgeInfo e) False (style l) (color l))
              st
              (catMaybes edges)
    returnA -< if kindStr == "HOST"
                then Just (HostGraph 1 "InitialGraph" st')
                else Nothing

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
