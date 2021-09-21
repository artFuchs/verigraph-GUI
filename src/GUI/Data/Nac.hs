module GUI.Data.Nac (
  MergeMapping
, NacInfo
, mergeElements
, applyNodeMerging
, applyEdgeMerging
, splitElements
, updateEdgeEndsIds
, addToGroup
, addToNAC
, removeFromNAC
, propagateRuleChanges
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




-- | Given the current nacInfo and the selected elements of the graph, merge the selected elements.
--   First step is to ensure the selected elements are in the right conditions for merging and to group them according by their types.
--   Second step is to merge the nodes, and third step is to merge the edges.
--   The merging follows the algorithm:
--    1. get the elements of the LHS
--    2. merge the elements of LHS
--    3. add the elements that are not in the lhs-nac morphism
mergeElements :: NacInfo -> ([NodeId],[EdgeId]) -> NacInfo
mergeElements ((g,gi),(nM,eM)) (nids, eids) = nacInfoFinal -- mergeElements' ((g,gi),(nM,eM)) (nidsToMerge, eidsToMerge)
  where
    nacInfoFinal = foldr (\eidsToMerge nacInfo -> mergeElements' nacInfo ([],eidsToMerge)) ((g',gi'),(nM',eM')) eidsGroups
    ((g',gi'),(nM',eM')) = foldr (\nidsToMerge nacInfo -> mergeElements' nacInfo (nidsToMerge,[])) ((g,gi),(nM,eM)) nidsGroups

    selectedNodes = Maybe.catMaybes $ map (\id -> lookupNode id g) nids
    nodeGroups = foldr (\n grps -> let t = infoType (nodeInfo n)
                                   in M.insertWith (++) t [n] grps) M.empty selectedNodes
    nodeGroups' = M.map (filter (infoLocked . nodeInfo)) nodeGroups
    nodeGroups'' = M.filter (\grp -> length grp > 1) nodeGroups'
    nidsGroups = M.elems $ M.map (map nodeId) nodeGroups''

    selectedEdges = Maybe.catMaybes $ map (\id -> lookupEdgeInContext id g') eids
    edgeGroups = foldr (\((src,_),e,(tgt,_)) grps ->
                            let t = infoType (edgeInfo e)
                                srcid = nodeId src
                                tgtid = nodeId tgt
                            in M.insertWith (++) (t,srcid,tgtid) [e] grps) M.empty selectedEdges
    edgeGroups' = M.map (filter (infoLocked . edgeInfo)) edgeGroups
    edgeGroups'' = M.filter (\grp -> length grp > 1) edgeGroups'
    eidsGroups = M.elems $ M.map (map edgeId) edgeGroups''





-- merge the elements
mergeElements' :: NacInfo -> ([NodeId],[EdgeId]) -> NacInfo
mergeElements' ((g,gi),(nM,eM)) (nids, eids) = ((g',gi'),(nM',eM'))
  where
    -- identify elements exclusive in the NAC
    nodesToKeep = filter (\n -> not . infoLocked . nodeInfo $ n) (nodes g)
    edgesToKeep = filter (\n -> not . infoLocked . edgeInfo $ n) (edges g)

    -- "restore" elements ids to the lhs for merging
    inverseNM = M.fromList $ map (\(a,b) -> (b,a)) $ M.toList nM
    inverseEM = M.fromList $ map (\(a,b) -> (b,a)) $ M.toList eM
    lhsNodes = Maybe.catMaybes $ map (\n -> (\nid -> n {nodeId = nid}) <$> M.lookup (nodeId n) inverseNM) (nodes g)
    lhsEdges = Maybe.catMaybes $ map (\e -> (\eid -> e {edgeId = eid}) <$> M.lookup (edgeId e) inverseEM) (edges g)

    -- merge the elements of LHS
    nM' = mergeElementsInMapping nM nids
    eM' = mergeElementsInMapping eM eids
    mergedNodes = applyNodeMerging lhsNodes nM'
    mergedEdges = applyEdgeMerging lhsEdges eM'

    -- add the elements that are not in the lhs-nac morphism
    nMAux = M.fromList . Maybe.catMaybes $ map (\(a,b) -> (\c -> (b,c)) <$> M.lookup a nM') (M.toList nM)
    newEdges = map (\e -> updateEdgeEndsIds e nMAux) (mergedEdges ++ edgesToKeep)
    g' = fromNodesAndEdges (mergedNodes ++ nodesToKeep) newEdges

    -- remove the graphical info of elements that are not in the new graph
    ngi' = M.filterWithKey (\k _ -> k `elem` (map fromEnum $ nodeIds g')) (fst gi)
    egi' = M.filterWithKey (\k _ -> k `elem` (map fromEnum $ edgeIds g')) (snd gi)
    gi' = (ngi',egi')


-- | Given an map of IDs to IDs and a target list, modify the mapping in a way that all the IDs that are mapped to an ID
--   in the target list become mapped to the maximun ID in the list
--
--   example: M.fromList [(1,1),(2,2),(3,3)] [1,3] = M.fromList [(1,3),(2,2),(3,3)]
--            M.fromList [(1,4),(2,5),(3,6)] [4,6] = M.fromList [(1,6),(2,5),(3,6)]
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
-- applyMerging [Node 3 "1 2 3"] [(1,3),(2,3),(3,3)] = [Node 3, " 1 2 3"]
applyNodeMerging :: [Node Info] -> M.Map NodeId NodeId -> [Node Info]
applyNodeMerging nodes mapping = mergedNodes
  where
    nodesGroups = foldr (addToGroup mapping nodeId) M.empty nodes
    mergeGroup k ns = Node k (mergeInfos $ map (\n -> (fromEnum $ nodeId n, nodeInfo n)) ns)
    mergedNodes = M.elems $ M.mapWithKey mergeGroup nodesGroups


-- | Given a list of edges and a edge merge mapping, merge the list of edges according to the mapping
-- similart to applyNodeMerging
applyEdgeMerging :: [Edge Info] -> M.Map EdgeId EdgeId -> [Edge Info]
applyEdgeMerging edges mapping = mergedEdges
  where
    edgesGroups = foldr (addToGroup mapping edgeId) M.empty edges
    mergeGroup k es = Edge
                    k
                    (sourceId $ head es)
                    (targetId $ head es)
                    (mergeInfos $ map (\e -> (fromEnum $ edgeId e, edgeInfo e)) es)
    mergedEdges = M.elems $ M.mapWithKey mergeGroup edgesGroups



-- | Merge a list of Info in one Info, with their labels separated by "\n ".
--   The rest of the data of first Info (type, operation, locked) in the list is used for the result.
--   examples: mergeInfos ["1{1}","2{1}","3{1}"] = "1\n2\n3{1}"
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
-- examples (syntax simplified for readability - using syntax of lists for Maps and using numbers for NodeIds):
--    addToGroup [(1, 2), (2, 2)] nodeId (Node 1) []             = [(2,[Node 1])]
--    addToGroup [(1, 2), (2, 2)] nodeId (Node 2) [(2,[Node 1])] = [(2,[Node 1, Node 2])]
addToGroup:: Ord k => Eq k => M.Map k k -> (a->k) -> a -> M.Map k [a] -> M.Map k [a]
addToGroup mapping getKey element groupMapping =
  let key = getKey element
  in case M.lookup key mapping of
    Nothing -> groupMapping
    Just k' -> M.insertWith (++) k' [element] groupMapping


-- | given a diagraph L (left side of the rule), a NAcInfo and the selected elements, split the merged selected elements
--   First step is to ensure the selected elements are merged
--   Second step is to split the elements, following the algorithm:
--    1. remove the elements that are merged and selected
--    2. identify which elements in L are not present anymore in the mapping and add those elements to the NAC
--    3. identify removed edges that were in between nodes of L and extra nodes on the NAC and add them
splitElements :: DiaGraph -> NacInfo -> ([NodeId],[EdgeId]) -> NacInfo
splitElements (l,lgi) ((nac,nacgi),(nM,eM)) (selN,selE) =
  if length nidsToSplit + length eidsToSplit > 0
    then splitElements' (l,lgi) ((nac,nacgi),(nM,eM)) (nidsToSplit,eidsToSplit)
    else ((nac,nacgi),(nM,eM))
  where
    mergedNids = getDuplicatedElems nM
    mergedEids = getDuplicatedElems eM
    nidsToSplit = filter (`elem` mergedNids) selN
    eidsToSplit = filter (`elem` mergedEids) selE

splitElements' :: DiaGraph -> NacInfo -> ([NodeId],[EdgeId]) -> NacInfo
splitElements' (l,lgi) ((nac,nacgi),(nM,eM)) (nidsToSplit,eidsToSplit) = newNacInfo
  where
    -- remove elements to split
    nacState = stateSetGraph nac . stateSetGI nacgi . stateSetSelected (nidsToSplit,eidsToSplit) $ emptyState
    nacState' = deleteSelected nacState
    nac' = stateGetGraph nacState'
    nacgi' = stateGetGI nacState'
    nM' = M.filter (`elem` (nodeIds nac')) nM
    eM' = M.filter (`elem` (edgeIds nac')) eM
    -- identify the elements in L that where removed and re-add them to the nac'
    removedLNids = filter (`notElem` M.keys nM') (nodeIds l)
    removedLEids = filter (`notElem` M.keys eM') (edgeIds l)
    nodesToAdd = map (\n -> let info = nodeInfo n in n { nodeInfo = infoSetLocked info True } ) $ filter (\n -> nodeId n `elem` removedLNids) (nodes l)
    edgesToAdd = map (\e -> let info = edgeInfo e in e { edgeInfo = infoSetLocked info True } ) $ filter (\e -> edgeId e `elem` removedLEids) (edges l)
    nodeGIsToAdd = M.filterWithKey (\k _ -> NodeId k `elem` removedLNids) (fst lgi)
    edgeGIsToAdd = M.filterWithKey (\k _ -> EdgeId k `elem` removedLEids) (snd lgi)
    ((nac'',nacgi''), (nM'',eM'')) = addToNAC (nac',nacgi') (nM',eM') (nodesToAdd,edgesToAdd) (nodeGIsToAdd,edgeGIsToAdd)
    -- re-add the edges removed from NAC that aren't in L
    edgesToKeep = filter (not . infoLocked . edgeInfo) (edges nac)
    edgesGIsToKeep = M.filterWithKey (\k _ -> k `elem` (map (fromEnum . edgeId) edgesToKeep)) (snd nacgi)
    ((newNac,newNacgi),_) = addToNAC (nac'',nacgi'') (M.empty,M.empty) ([],edgesToKeep) (M.empty,edgesGIsToKeep)
    newNacInfo = ((newNac,newNacgi), (nM'',eM''))



-- add elements to NAC diagraph, updating the mapping of elements from LHS to NAC
addToNAC :: DiaGraph -> MergeMapping -> ([Node Info], [Edge Info]) -> GraphicalInfo -> NacInfo
addToNAC (nac,nacgi) (nM,eM) (nodesToAdd,edgesToAdd) (nodeGIsToAdd,edgeGIsToAdd) = ((nac'',nacgi'),(nM',eM'))
  where
    nodesAndGIsToAdd = map (\n -> (n, getNodeGI (fromEnum $ nodeId n) nodeGIsToAdd)) nodesToAdd
    edgesAndGIsToAdd = map (\e -> (e, getEdgeGI (fromEnum $ edgeId e) edgeGIsToAdd)) edgesToAdd
    (nac',nacNgi', nM') = foldr addNode (nac, fst nacgi, nM) nodesAndGIsToAdd
    (nac'',nacEgi', eM') = foldr (addEdge nM') (nac', snd nacgi, eM) edgesAndGIsToAdd
    nacgi' = (nacNgi',nacEgi')

-- add Node and it's Layout to NAC diagraph, updating the node mapping
addNode :: (Node Info, NodeGI) -> (Graph Info Info, M.Map Int NodeGI, M.Map NodeId NodeId) -> (Graph Info Info, M.Map Int NodeGI, M.Map NodeId NodeId)
addNode (n,ngi) (nac, nacNgi, nM) = (nac', nacNgi', nM')
  where
    nid' =  if (nodeId n) `elem` (nodeIds nac)
              then head $ newNodes nac
              else nodeId n
    nac' = insertNodeWithPayload nid' (nodeInfo n) nac
    nacNgi' = M.insert (fromEnum nid') ngi nacNgi
    nM' = M.insert (nodeId n) nid' nM

-- add Edge and it's Layout to NAC diagraph, updating the edge mapping
addEdge :: M.Map NodeId NodeId -> (Edge Info, EdgeGI) -> (Graph Info Info, M.Map Int EdgeGI, M.Map EdgeId EdgeId) -> (Graph Info Info, M.Map Int EdgeGI, M.Map EdgeId EdgeId)
addEdge nM (e,egi) (nac, nacEgi, eM) = (nac', nacEgi', eM')
  where
    eid' = if (edgeId e) `elem` (edgeIds nac)
              then head $ newEdges nac
              else edgeId e
    e' = (updateEdgeEndsIds e nM)
    nac' = insertEdgeWithPayload eid' (sourceId e') (targetId e') (edgeInfo e) nac
    nacEgi' = M.insert (fromEnum eid') egi nacEgi
    eM' = M.insert (edgeId e) eid' eM

-- remove the specified elements from the NAC
removeFromNAC :: NacInfo -> ([NodeId], [EdgeId]) -> NacInfo
removeFromNAC ((nac,nacgi),(nM,eM)) (nodesToRemove, edgesToRemove) = ((nac',nacgi),(nM',eM'))
  where
    nac' = fromNodesAndEdges nacNodes nacEdges
    nacgi' = (nacNGIs, nacEGIs)
    nM' = M.filterWithKey (\k _ -> k `notElem` nodesToRemove) nM
    eM' = M.filterWithKey (\k _ -> k `notElem` edgesToRemove) eM
    nacNodes = filter (\n -> (not . infoLocked . nodeInfo $ n) || (nodeId n `elem` (M.elems nM'))) (nodes nac)
    nacEdges = filter (\e -> (not . infoLocked . edgeInfo $ e) || (edgeId e `elem` (M.elems eM'))) (edges nac)
    nacNGIs = M.filterWithKey (\k _ -> (NodeId k) `elem` (map nodeId nacNodes)) (fst nacgi)
    nacEGIs = M.filterWithKey (\k _ -> (EdgeId k) `elem` (map edgeId nacEdges)) (snd nacgi)


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



-- | Given a LHS and a NAC, modify the NAC in a way o reflect changes that may have occurred in the LHS
propagateRuleChanges :: DiaGraph -> NacInfo -> NacInfo
propagateRuleChanges (lhs,lhsgi) ((nac,nacgi),(nm,em)) = ((nacFinal,nacgi2), (nm'',em''))
  where
    -- 1 remove from NAC elements not present on LHS
    nodesToRemove = filter (`notElem` (nodeIds lhs)) (M.keys nm)
    edgesToRemove = filter (`notElem` (edgeIds lhs)) (M.keys em)
    ((nac1,nacgi1),(nm',em')) = removeFromNAC ((nac,nacgi),(nm,em)) (nodesToRemove,edgesToRemove)
    -- 2 add to the NAC the elements present on LHS but not on the NAC
    nodesToAdd = map (\n -> let info = nodeInfo n in n { nodeInfo = infoSetLocked info True } ) $ filter (\n -> nodeId n `notElem` (M.keys nm)) (nodes lhs)
    edgesToAdd = map (\e -> let info = edgeInfo e in e { edgeInfo = infoSetLocked info True } ) $ filter (\e -> edgeId e `notElem` (M.keys em)) (edges lhs)
    nodeGIsToAdd = M.filterWithKey (\k _ -> NodeId k `notElem` (M.keys nm)) (fst lhsgi)
    edgeGIsToAdd = M.filterWithKey (\k _ -> EdgeId k `notElem` (M.keys em)) (snd lhsgi)
    ((nac2,nacgi2), (nm'',em'')) = addToNAC (nac1,nacgi1) (nm',em') (nodesToAdd,edgesToAdd) (nodeGIsToAdd,edgeGIsToAdd)
    -- 3 ensure the types and labels of the elements of the rule and the nac corresponds
    lhsNodesMerged = map (\n -> let info = nodeInfo n in n { nodeInfo = infoSetLocked info True } ) $  applyNodeMerging (nodes lhs) nm''
    lhsEdgesMerged = map (\e -> let info = edgeInfo e in e { edgeInfo = infoSetLocked info True } ) $  applyEdgeMerging (edges lhs) em''
    nac3 = foldr (\n g -> updateNodePayload (nodeId n) g (\_ -> nodeInfo n)) nac2 lhsNodesMerged
    nacFinal = foldr (\e g -> updateEdgePayload (edgeId e) g (\_ -> edgeInfo e)) nac3 lhsEdgesMerged
