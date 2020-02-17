module Editor.Data.Nac (
  MergeMapping
, NacInfo
, extractNacGraph
, extractNacGI
, remapElementsWithConflict
, updateEdgeEndsIds
, addToGroup
)where

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Data.List

import Abstract.Rewriting.DPO
import Data.Graphs

import Editor.Data.DiaGraph
import Editor.Data.GraphicalInfo
import Editor.Data.Info1
import Editor.Helper.List

type MergeMapping = (M.Map NodeId NodeId, M.Map EdgeId EdgeId)
type NacInfo = (DiaGraph, MergeMapping)

-- | Given a Graph and a MergeMapping, extract the elements that are part of the nac subgraph that contains merged and added elements
-- this function also merges the elements as defined in the MergeMapping
extractNacGraph :: Graph Info Info -> MergeMapping -> Graph Info Info
extractNacGraph g (nM, eM) = fromNodesAndEdges nacNodes nacEdges
  where
    lhsNodes = filter (\n -> infoLocked (nodeInfo n)) (nodes g)
    lhsSelectedNodes = applyNodeMerging lhsNodes nM
    addedNodes = filter (\n -> not $ infoLocked (nodeInfo n)) (nodes g)
    nacNodes = lhsSelectedNodes ++ addedNodes
    lhsEdges = filter (\e -> infoLocked (edgeInfo e)) (edges g)
    lhsSelectedEdges = applyEdgeMerging lhsEdges eM
    addedEdges = filter (\e -> not $ infoLocked (edgeInfo e)) (edges g)
    nacEdges = map (\e -> updateEdgeEndsIds e nM) (lhsSelectedEdges ++ addedEdges)

extractNacGI :: Graph Info Info -> GraphicalInfo -> MergeMapping -> GraphicalInfo
extractNacGI g (ngi,egi) (nM,eM) = (ngi',egi')
  where
    addedNids = map (fromEnum . nodeId) $ filter (\n -> not $ infoLocked (nodeInfo n)) (nodes g)
    addedEids = map (fromEnum . edgeId) $ filter (\e -> not $ infoLocked (edgeInfo e)) (edges g)
    mergedNids = map fromEnum $ M.elems nM
    mergedEids = map fromEnum $ M.elems eM
    addedNgi = M.filterWithKey (\k a -> k `elem` addedNids) ngi
    mergedNgi = M.filterWithKey (\k a -> k `elem` mergedNids) ngi
    ngi' = M.union addedNgi mergedNgi
    egi' = M.filterWithKey (\k a -> k `elem` addedEids || k `elem` mergedEids) egi


-- function to modify ids of nac nodes that aren't in mapping and have id conflict with elems from lhs
-- (g1,(ngi1,egi1)) must be the lhs diagraph, while (g2,(ngi2,egi2)) must be the nac diagraph
remapElementsWithConflict :: DiaGraph -> DiaGraph -> MergeMapping -> DiaGraph
remapElementsWithConflict (g1,(ngi1,egi1)) (g2,(ngi2,egi2)) (nodeMapping, edgeMapping) = (g3,gi3)
  where
    g3 = fromNodesAndEdges g3Nodes g3Edges
    gi3 = (ngi3,egi3)
    egi3 = M.fromList $ map (\(k,a) -> let k' = fromEnum . Maybe.fromMaybe (EdgeId k) $ M.lookup (EdgeId k) newEids
                                        in (k',a))
                            (M.toList egi2)
    ngi3 = M.fromList $ map (\(k,a) -> let k' = fromEnum . Maybe.fromMaybe (NodeId k) $ M.lookup (NodeId k) newNids
                                        in (k',a))
                            (M.toList ngi2)
    possibleNConflicts1 = filter (\n -> notElem n (M.keys nodeMapping)) $ nodeIds g1
    possibleNConflicts2 = filter (\n -> notElem n (M.elems nodeMapping)) $ nodeIds g2
    possibleEConflicts1 = filter (\e -> notElem e (M.keys edgeMapping)) $ edgeIds g1
    possibleEConflicts2 = filter (\e -> notElem e (M.elems edgeMapping)) $ edgeIds g2
    nConflicts = filter (\(a,b) -> a == b) $ mkpairs possibleNConflicts1 possibleNConflicts2
    eConflicts = filter (\(a,b) -> a == b) $ mkpairs possibleEConflicts1 possibleEConflicts2
    newNids = M.fromList $ zipWith (\(a,b) n -> (a, n)) nConflicts (newNodes g1)
    newEids = M.fromList $ zipWith (\(a,b) n -> (a, n)) eConflicts (newEdges g1)
    g3Nodes = map (\n -> let nid = Maybe.fromMaybe (nodeId n) $ M.lookup (nodeId n) newNids
                          in Node nid (nodeInfo n))
                  (nodes g2)
    g3Edges = map (\e -> let srcNid = Maybe.fromMaybe (sourceId e) $ M.lookup (sourceId e) newNids
                             tgtNid = Maybe.fromMaybe (targetId e) $ M.lookup (targetId e) newNids
                             eid = Maybe.fromMaybe (edgeId e) $ M.lookup (edgeId e) newEids
                          in Edge eid srcNid tgtNid (edgeInfo e))
                  (edges g2)

-- mergeInfos: function to merge a list of Info in one Info, with their labels separated by "\n ".
--   The rest of the data of first Info (type, operation, locked) in the list is used for the result
-- examples: mergeInfos ["1{1}","2{1}","3{1}"] = "1\n2\n3{1}"
--           mergeInfos ["1{1}","2{2}","3{3}"] = "1\n2\n3{1}"
mergeInfos :: [(Int,Info)] -> Info
mergeInfos infos = newInfo
  where
    (k,i):is = reverse $ sortOn fst infos
    newInfo = foldr
            (\(k,i) info -> case infoLabel i of
              Label str -> infoAddLabel info k str
              LabelGroup lbls -> foldr
                                  (\(k',str) info' -> case k of
                                    0 -> infoAddLabel info' k str
                                    n -> infoAddLabel info' k' str
                                  )
                                  info lbls
            )
            i is


-- | Given a mapping of element keys to group keys , a function to extract a key from an element,
-- the element and a mapping of keys to lists of elements (aka. groups),
-- add the element to the group indicated by the mapping
addToGroup:: Ord k => Eq k => M.Map k k -> (a->k) -> a -> M.Map k [a] -> M.Map k [a]
addToGroup mapping getKey element groupMapping =
  let key = getKey element
  in case M.lookup key mapping of
    Nothing -> groupMapping
    Just k' -> M.insertWith (++) k' [element] groupMapping


-- given a list of nodes and a node merge mapping, merge the list of nodes according to the mapping
-- examples: 
-- applyMerging [Node 1 "1", Node 2 "2", Node 3 "3"]
--              [(1,3),(2,3),(3,3)]
-- = [Node 3 "1 2 3"]
-- 
-- applyMerging [Node 3 "1,2,3"]
--              []
-- = []
--
-- applyMerging [Node 3 "1,2,3"]
--              [(3,3)]
-- = [Node 3 "3"]
applyNodeMerging :: [Node Info] -> M.Map NodeId NodeId -> [Node Info]
applyNodeMerging nodes mapping = mergedNodes
  where
    nodesGroups = M.elems $ foldr (addToGroup mapping nodeId) M.empty nodes
    mergeGroup ns = Node
                    (maximum $ map nodeId ns)
                    (mergeInfos $ map (\n -> (fromEnum $ nodeId n, nodeInfo n)) ns)
    splitNode n = Node
                    (nodeId n)
                    (infoSetLabel (nodeInfo n) $ infoOriginalLabel $ nodeInfo n)
    mergeOrSplit ns = case ns of
                        n:[] -> case length (M.filter (== (nodeId n)) mapping) > 1 of
                          True -> n
                          False -> splitNode n
                        _ -> mergeGroup ns
    mergedNodes = map mergeOrSplit nodesGroups


-- given a list of edges and a edge merge mapping, merge the list of edges according to the mapping
-- examples
applyEdgeMerging :: [Edge Info] -> M.Map EdgeId EdgeId -> [Edge Info]
applyEdgeMerging edges mapping = mergedEdges
  where
    edgesGroups = M.elems $ foldr (addToGroup mapping edgeId) M.empty edges
    mergeGroup es = Edge
                    (maximum $ map edgeId es)
                    (sourceId $ head es)
                    (targetId $ head es)
                    (mergeInfos $ map (\e -> (fromEnum $ edgeId e, edgeInfo e)) es)
    mergedEdges = map mergeGroup edgesGroups

updateNodeId :: NodeId -> M.Map NodeId NodeId -> NodeId
updateNodeId nid m = Maybe.fromMaybe nid $ M.lookup nid m

updateEdgeId :: EdgeId -> M.Map EdgeId EdgeId -> EdgeId
updateEdgeId eid m = Maybe.fromMaybe eid $ M.lookup eid m

updateEdgeEndsIds :: Edge a -> M.Map NodeId NodeId -> Edge a
updateEdgeEndsIds e m = Edge (edgeId e) (updateNodeId (sourceId e) m) (updateNodeId (targetId e) m) (edgeInfo e)
