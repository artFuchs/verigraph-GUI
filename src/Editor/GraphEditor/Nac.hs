module Editor.GraphEditor.Nac (
  MergeMapping
, NacInfo
, extractNacGraph
, extractNacGI
, remapElementsWithConflict
, updateEdgeEndsIds
)where

import qualified Data.Map as M
import qualified Data.Maybe as Maybe

import Abstract.Rewriting.DPO
import Data.Graphs

import Editor.Data.DiaGraph
import Editor.Data.GraphicalInfo
import Editor.Data.Info
import Editor.Helper.List

type MergeMapping = (M.Map NodeId NodeId, M.Map EdgeId EdgeId)
type NacInfo = (DiaGraph, MergeMapping)


--
extractNacGraph :: Graph Info Info -> MergeMapping -> Graph Info Info
extractNacGraph g (nM, eM) = fromNodesAndEdges nacNodes nacEdges
  where
    lhsNodes = filter (\n -> infoLocked (nodeInfo n)) (nodes g)
    lhsSelectedNodes = filter (\n -> nodeId n `elem` (M.elems nM)) lhsNodes
    addedNodes = filter (\n -> not $ infoLocked (nodeInfo n)) (nodes g)
    nacNodes = lhsSelectedNodes ++ addedNodes
    lhsEdges = filter (\e -> infoLocked (edgeInfo e)) (edges g)
    lhsSelectedEdges = filter (\e -> edgeId e `elem` (M.elems eM)) lhsEdges
    addedEdges = filter (\e -> not $ infoLocked (edgeInfo e)) (edges g)
    nacEdges = map (\e -> updateEdgeEndsIds e nM) (lhsSelectedEdges ++ addedEdges)

extractNacGI :: Graph Info Info -> GraphicalInfo -> MergeMapping -> GraphicalInfo
extractNacGI g (ngi,egi) (nM,eM)  = (ngi',egi')
  where
    lhsNids = map nodeId $ filter (\n -> infoLocked (nodeInfo n)) (nodes g)
    lhsEids = map edgeId $ filter (\e -> infoLocked (edgeInfo e)) (edges g)
    ngi' = M.filterWithKey (\k a -> NodeId k `notElem` lhsNids) ngi
    egi' = M.filterWithKey (\k a -> EdgeId k `notElem` lhsEids) egi

-- function to modify ids of nac nodes that aren't in mapping and have id conflict with elems from lhs
remapElementsWithConflict :: DiaGraph -> DiaGraph -> (M.Map NodeId NodeId, M.Map EdgeId EdgeId) -> DiaGraph
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


updateNodeId :: NodeId -> M.Map NodeId NodeId -> NodeId
updateNodeId nid m = Maybe.fromMaybe nid $ M.lookup nid m

updateEdgeId :: EdgeId -> M.Map EdgeId EdgeId -> EdgeId
updateEdgeId eid m = Maybe.fromMaybe eid $ M.lookup eid m

updateEdgeEndsIds :: Edge a -> M.Map NodeId NodeId -> Edge a
updateEdgeEndsIds e m = Edge (edgeId e) (updateNodeId (sourceId e) m) (updateNodeId (targetId e) m) (edgeInfo e)
