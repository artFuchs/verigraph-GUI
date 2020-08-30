module GUI.Editor.Helper.Clipboard(
  copySelected
, pasteClipBoard
)where

import qualified Data.Map as M

import Data.Graphs

import GUI.Data.DiaGraph
import GUI.Data.GraphState
import GUI.Data.Info
import GUI.Data.GraphicalInfo


-- | generate a DiaGraph with the selected elements from a given GraphState
copySelected :: GraphState -> DiaGraph
copySelected  es = (cg,(ngiM',egiM'))
  where
    (nids,eids) = stateGetSelected es
    g = stateGetGraph es
    (ngiM, egiM) = stateGetGI es
    cnodes = foldr (\n ns -> if nodeId n `elem` nids
                              then (Node (nodeId n) (infoSetLocked (nodeInfo n) False)):ns
                              else ns) [] (nodes g)
    cedges = foldr (\e es -> if edgeId e `elem` eids
                              then (Edge (edgeId e) (sourceId e) (targetId e) (infoSetLocked (edgeInfo e) False):es)
                              else es) [] (edges g)
    cg = fromNodesAndEdges cnodes cedges
    ngiM' = M.filterWithKey (\k _ -> NodeId k `elem` nids) ngiM
    egiM' = M.filterWithKey (\k _ -> EdgeId k `elem` eids) egiM

-- | join a DiaGraph with the Diagraph of a given GraphState
pasteClipBoard :: DiaGraph -> GraphState -> GraphState
pasteClipBoard clipDG es = stateSetGI (newngiM,newegiM) . stateSetGraph newGraph . stateSetSelected ([], [])$ es
  where
    graph = stateGetGraph es
    (ngiM, egiM) = stateGetGI es
    (newGraph, (newngiM,newegiM)) = diagrDisjointUnion (graph,(ngiM,egiM)) (adjustDiagrPosition clipDG)  