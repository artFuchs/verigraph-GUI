module GUI.Data.Nac (
  MergeMapping
, NacInfo
, extractNac
, extractNacGraph
, extractNacGI
, mergeElements
, updateEdgeEndsIds
, addToGroup
)where

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Data.List

import Abstract.Rewriting.DPO
import Data.Graphs

import GUI.Data.DiaGraph
import GUI.Data.GraphicalInfo
import GUI.Data.Info
import GUI.Data.GraphState
import GUI.Helper.List

type MergeMapping = (M.Map NodeId NodeId, M.Map EdgeId EdgeId)
type NacInfo = (DiaGraph, MergeMapping)


extractNac :: Graph Info Info -> GraphicalInfo -> MergeMapping -> NacInfo
extractNac g gi (nM,eM) = ((nacG,nacGI),mergeM)
  where
    lhsEdges = filter (\e -> infoLocked (edgeInfo e)) (edges g)
    lhsNodes = filter (\n -> infoLocked (nodeInfo n)) (nodes g)
    lhsMergedNodes = applyNodeMerging lhsNodes nM
    lhsNodesToKeep =
        filter (\n -> (nodeId n) `elem` (map sourceId addedEdges) ||
                      (nodeId n) `elem` (map targetId addedEdges) )
               lhsNodes
    lhsSelectedNodes = lhsMergedNodes ++ lhsNodesToKeep
    addedNodes = filter (\n -> not $ infoLocked (nodeInfo n)) (nodes g)
    nacNodes = lhsSelectedNodes ++ addedNodes

    nM' = foldr (\n m -> if n `elem` M.keys m then
                            m
                         else
                           M.insert n n m)
                nM $ map nodeId lhsNodesToKeep

    lhsSelectedEdges = applyEdgeMerging lhsEdges eM
    addedEdges = filter (\e -> not $ infoLocked (edgeInfo e)) (edges g)
    nacEdges = map (\e -> updateEdgeEndsIds e nM') (lhsSelectedEdges ++ addedEdges)


    mergeM = (nM', eM)
    nacG = fromNodesAndEdges nacNodes nacEdges
    nacGI = extractNacGI g gi mergeM


-- | Given a Graph and a MergeMapping, extract the elements that are part of the nac subgraph that contains merged
--   and added elements.
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

-- | Given a Graph, a GraphicalInfo and a MergeMapping, extract the graphical information that are part of the nac.
extractNacGI :: Graph Info Info -> GraphicalInfo -> MergeMapping -> GraphicalInfo
extractNacGI g (ngi,egi) (nM,eM) = (ngi',egi')
  where
    addedNids = map (fromEnum . nodeId) $ filter (\n -> not $ infoLocked (nodeInfo n)) (nodes g)
    addedEids = map (fromEnum . edgeId) $ filter (\e -> not $ infoLocked (edgeInfo e)) (edges g)
    mergedNids = map fromEnum $ M.elems nM
    mergedEids = map fromEnum $ M.elems eM
    ngi' = M.filterWithKey (\k a -> k `elem` addedNids || k `elem` mergedNids) ngi
    egi' = M.filterWithKey (\k a -> k `elem` addedEids || k `elem` mergedEids) egi




mergeElements :: NacInfo -> ([NodeId],[EdgeId]) -> NacInfo
mergeElements ((g,gi),(nM,eM)) (nids, eids) = mergeElements' ((g,gi),(nM,eM)) (nidsToMerge, eidsToMerge)
  where
    nidsToMerge = map (\n -> nodeId n) nodesToMerge
    nodesToMerge = filter (\n -> Just (infoType (nodeInfo n)) == nodeType) selectedNodes
    selectedNodes = Maybe.catMaybes $ map (\id -> lookupNode id g) nids
    nodeType = case selectedNodes of
                [] -> Nothing
                n:_ -> Just (infoType . nodeInfo $ n)
    eidsToMerge = map (\e -> edgeId e) edgesToMerge
    edgesToMerge = filter (\e -> Just (infoType (edgeInfo e)) == edgeType) selectedEdges
    selectedEdges = Maybe.catMaybes $ map (\id -> lookupEdge id g) eids
    edgeType = case selectedEdges of
                [] -> Nothing
                e:_ -> Just (infoType . edgeInfo $ e)


mergeElements' :: NacInfo -> ([NodeId],[EdgeId]) -> NacInfo
mergeElements' ((g,gi),(nM,eM)) (nids, eids) = ((g',gi'),(nM',eM'))
  where
    nodesToKeep = filter (\n -> not . infoLocked . nodeInfo $ n) (nodes g)
    edgesToKeep = filter (\n -> not . infoLocked . edgeInfo $ n) (edges g)
    mergedNodes = applyNodeMerging (nodes g) nM'
    mergedEdges = applyEdgeMerging (edges g) eM'
    nM' = mergeElementsInMapping nM nids
    eM' = mergeElementsInMapping eM eids
    newEdges = map (\e -> updateEdgeEndsIds e nM') (mergedEdges ++ edgesToKeep)
    g' = fromNodesAndEdges (mergedNodes ++ nodesToKeep) newEdges
    ngi' = M.filterWithKey (\k _ -> k `elem` (map fromEnum $ nodeIds g')) (fst gi)
    egi' = M.filterWithKey (\k _ -> k `elem` (map fromEnum $ edgeIds g')) (snd gi)
    gi' = (ngi',egi')


-- given an map of IDs to IDs and a target list, modify the mapping in a way that all the IDs that are mapped to an ID
-- in the target list become mapped to the maximun ID in the list
-- example: M.fromList [(1,1),(2,2),(3,3)] [1,3] = M.fromList [(1,3),(2,2),(3,3)]
--          M.fromList [(1,4),(2,5),(3,6)] [4,6] = M.fromList [(1,6),(2,5),(3,6)]
mergeElementsInMapping :: (Num n, Eq n, Ord n) => M.Map n n -> [n] -> M.Map n n
mergeElementsInMapping mapping idsToMerge = mapping'
  where
    targetId = maximum idsToMerge
    getKeysMappingTo id = M.keys $ M.filter (\a -> a == id) mapping
    idsToMerge' = concat $ map getKeysMappingTo $ filter (\id -> id `elem` M.elems mapping) idsToMerge
    mapping' = foldr (\id m -> M.insert id targetId m) mapping idsToMerge'


-- | Given a list of nodes and a node merge mapping, merge the list of nodes according to the mapping
-- examples:
-- applyMerging [Node 1 "1", Node 2 "2", Node 3 "3"] [(1,3),(2,3),(3,3)] = [Node 3 "1 2 3"]
-- applyMerging [Node 1 "1", Node 2 "2", Node 3 "3"] []                  = []
-- applyMerging [Node 1 "1", Node 2 "2", Node 3 "3"] [(3,3)]             = [Node 3 "3"]
-- applyMerging [Node 1 "1", Node 2 "2", Node 3 "3"] [(1,1),(2,2),(3,3)] = [Node 1 "1", Node 2 "2", Node 3 "3"]
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


-- | Given a list of edges and a edge merge mapping, merge the list of edges according to the mapping
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



-- | Merge a list of Info in one Info, with their labels separated by "\n ".
--   The rest of the data of first Info (type, operation, locked) in the list is used for the result.
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


splitElements :: DiaGraph -> NacInfo -> ([NodeId],[EdgeId]) -> NacInfo
splitElements (l,lgi) ((nac,nacgi),(nM,eM)) (selN,selE) = newNacInfo
  where
    -- remove selected elements that are merged from nac
    mergedNids = getDuplicatedElems nM
    mergedEids = getDuplicatedElems eM
    nidsToRemove = filter (`elem` mergedNids) selN
    eidsToRemove = filter (`elem` mergedEids) selE
    nacState = deleteSelected (stateSetGraph nac . stateSetGI nacgi . stateSetSelected (nidsToRemove,eidsToRemove) $ emptyState)
    nac' = stateGetGraph nacState
    nacgi' = stateGetGI nacState
    nM' = M.filter (`elem` (nodeIds nac')) nM
    eM' = M.filter (`elem` (edgeIds nac')) eM
    -- identify elements in l that are not present in the mapping
    removedLNids = filter (`notElem` M.keys nM) (nodeIds l)
    removedLEids = filter (`notElem` M.keys eM) (edgeIds l)
    -- add removed elements to nac'
    nodesToAdd = filter (\n -> nodeId n `elem` removedLNids) (nodes l)
    edgesToAdd = filter (\e -> edgeId e `elem` removedLEids) (edges l)
    nodeGIsToAdd = M.filterWithKey (\k _ -> NodeId k `elem` removedLNids) (fst lgi)
    edgeGIsToAdd = M.filterWithKey (\k _ -> EdgeId k `elem` removedLEids) (snd lgi)
    newNacInfo = addToNAC (nac',nacgi') (nM',eM') (nodesToAdd,edgesToAdd) (nodeGIsToAdd,edgeGIsToAdd)



-- TODO: implement this function
addToNAC :: DiaGraph -> MergeMapping -> ([Node Info], [Edge Info]) -> GraphicalInfo -> NacInfo
addToNAC (nac,nacgi) (nM,eM) (nodesToAdd,edgesToAdd) (nodeGIsToAdd,edgeGIsToAdd) = ((nac,nacgi),(nM,eM))


getDuplicatedElems :: (Ord a) => M.Map a a -> [a]
getDuplicatedElems m = duplicated
  where
    duplicated = map fst $ filter (\(a,c) -> c > 1) $ M.toList elemCount
    elemCount = M.foldr (\a m -> M.insertWith (+) a 1 m) M.empty m


updatedNodeId :: NodeId -> M.Map NodeId NodeId -> NodeId
updatedNodeId nid m = Maybe.fromMaybe nid $ M.lookup nid m

updatedEdgeId :: EdgeId -> M.Map EdgeId EdgeId -> EdgeId
updatedEdgeId eid m = Maybe.fromMaybe eid $ M.lookup eid m

updateEdgeEndsIds :: Edge a -> M.Map NodeId NodeId -> Edge a
updateEdgeEndsIds e m = Edge (edgeId e) (updatedNodeId (sourceId e) m) (updatedNodeId (targetId e) m) (edgeInfo e)
