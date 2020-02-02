module Editor.Data.Nac (
  MergeMapping
, NacInfo
, extractNacGraph
, extractNacGI
, remapElementsWithConflict
, updateEdgeEndsIds
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

extractNacGraph :: Graph Info Info -> MergeMapping -> Graph Info Info
extractNacGraph g (nM, eM) = fromNodesAndEdges nacNodes nacEdges
  where
    lhsNodes = filter (\n -> infoLocked (nodeInfo n)) (nodes g)
    --lhsSelectedNodes = filter (\n -> nodeId n `elem` (M.elems nM)) lhsNodes
    lhsSelectedNodes = mergeNodes lhsNodes nM
    addedNodes = filter (\n -> not $ infoLocked (nodeInfo n)) (nodes g)
    nacNodes = lhsSelectedNodes ++ addedNodes
    lhsEdges = filter (\e -> infoLocked (edgeInfo e)) (edges g)
    --lhsSelectedEdges = filter (\e -> edgeId e `elem` (M.elems eM)) lhsEdges
    lhsSelectedEdges = mergeEdges lhsEdges eM
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

-- | mergeNodes: function to merge nodes, joining their infos
-- example: mergeNodes [Node 1 "1", Node 2 "2", Node 3 "3", Node 4 "4", Node 5 "5"] 
--                     [(1,3),(2,3),(3,3),(4,5),(5,5)] 
--          = [Node 3 "1 2 3", Node 5 "4 5"
mergeNodes :: [Node Info] -> M.Map NodeId NodeId -> [Node Info]
mergeNodes nodes mapping = mergedNodes
  where
    group n m = let nid = nodeId n
                in case M.lookup nid mapping of
                    Nothing -> m
                    Just nid' -> case M.lookup nid' m of
                                  Nothing -> M.insert nid' [n] m
                                  Just ns -> M.insert nid' (n:ns) m
    nodesGroups = M.elems $ foldr group M.empty nodes
    mergeGroup ns = Node 
                    (maximum $ map nodeId ns) 
                    (mergeInfos $ map (\n -> (fromEnum $ nodeId n, nodeInfo n)) ns)
    splitNode n = Node 
                    (nodeId n) 
                    (infoSetLabel (nodeInfo n) $ infoOriginalLabel $ nodeInfo n)
    mergeOrSplit ns = case ns of 
                        n:[] -> splitNode n 
                        _ -> mergeGroup ns
    mergedNodes = map mergeOrSplit nodesGroups

mergeEdges :: [Edge Info] -> M.Map EdgeId EdgeId -> [Edge Info]
mergeEdges edges mapping = mergedEdges
  where
    group e m = let eid = edgeId e
                in case M.lookup eid mapping of
                  Nothing -> m
                  Just eid' -> case M.lookup eid' m of
                                Nothing -> M.insert eid' [e] m
                                Just es -> M.insert eid' (e:es) m
    edgesGroups = M.elems $ foldr group M.empty edges
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
