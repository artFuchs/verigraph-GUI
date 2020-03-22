module Editor.GraphEditor.Helper.Clipboard(
  copySelected
, pasteClipBoard
)where

import qualified Data.Map as M

import Data.Graphs

import Editor.Data.DiaGraph
import Editor.Data.EditorState
import Editor.Data.Info1
import Editor.Data.GraphicalInfo


-- | generate a DiaGraph with the selected elements from a given EditorState
copySelected :: EditorState -> DiaGraph
copySelected  es = (cg,(ngiM',egiM'))
  where
    (nids,eids) = editorGetSelected es
    g = editorGetGraph es
    (ngiM, egiM) = editorGetGI es
    cnodes = foldr (\n ns -> if nodeId n `elem` nids
                              then (Node (nodeId n) (infoSetLocked (nodeInfo n) False)):ns
                              else ns) [] (nodes g)
    cedges = foldr (\e es -> if edgeId e `elem` eids
                              then (Edge (edgeId e) (sourceId e) (targetId e) (infoSetLocked (edgeInfo e) False):es)
                              else es) [] (edges g)
    cg = fromNodesAndEdges cnodes cedges
    ngiM' = M.filterWithKey (\k _ -> NodeId k `elem` nids) ngiM
    egiM' = M.filterWithKey (\k _ -> EdgeId k `elem` eids) egiM

-- | join a DiaGraph with the Diagraph of a given EditorState
pasteClipBoard :: DiaGraph -> EditorState -> EditorState
pasteClipBoard (cGraph, (cNgiM, cEgiM)) es = editorSetGI (newngiM,newegiM) . editorSetGraph newGraph . editorSetSelected ([], [])$ es
  where
    graph = editorGetGraph es
    (ngiM, egiM) = editorGetGI es
    allPositions = concat [map position (M.elems cNgiM), map (getEdgePosition cGraph (cNgiM, cEgiM)) (edges cGraph)]
    minX = minimum $ map fst allPositions
    minY = minimum $ map snd allPositions
    upd (a,b) = (20+a-minX, 20+b-minY)
    cNgiM' = M.map (\gi -> nodeGiSetPosition (upd $ position gi) gi) cNgiM
    (newGraph, (newngiM,newegiM)) = diagrDisjointUnion (graph,(ngiM,egiM)) (cGraph,(cNgiM', cEgiM))