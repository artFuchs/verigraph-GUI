module GUI.Executor.MatchApplier (
  applyMatch
, Match
, TGMProduction

)where

import qualified  Data.Map as M
import            Data.Maybe
import            Data.Int

import            Abstract.Rewriting.DPO             hiding (productions)
import qualified  Abstract.Rewriting.DPO   as DPO
import qualified  Data.Graphs as G
import qualified  Data.Graphs.Morphism     as GM
import qualified  Data.TypedGraph.Morphism as TGM
import qualified  Data.Relation            as R

import            GUI.Data.GraphicalInfo
import            GUI.Data.GraphState
import            GUI.Data.Info
import            GUI.Helper.Geometry
import qualified  GUI.Helper.GrammarMaker  as GMker
import            GUI.Helper.OverlapAvoider

type Match = TGM.TypedGraphMorphism Info Info
type TGMProduction = DPO.Production (TGM.TypedGraphMorphism Info Info)

-- | apply a match in a graphState, generating a new one.
-- It ensures that he payloads of nodes and edges of the final graph are correct and
-- updates the GI positioning new nodes relatively to the position it has in the rule.
applyMatch :: GraphState -> M.Map Int32 GraphState -> Int32 -> TGMProduction -> Match -> GraphState
applyMatch hostSt statesM rIndex p m = hState
  where
    -- calculate graph
    (k,n,f,g) = DPO.calculateDPO m p

    gNodes = map (\(G.Node i l) -> G.Node i (Just l) ) $ G.nodes (stateGetGraph hostSt)
    gEdges = map (\(G.Edge i s t l) -> G.Edge i s t (Just l)) $ G.edges (stateGetGraph hostSt)
    gGraph = G.fromNodesAndEdges gNodes gEdges

    (finalGraph, dGraph, hGraph) = applyMatchGraph [k,n,f,g] gGraph
    -- modify graphical information to match the modifications
    hState = applyMatchGI finalGraph dGraph hGraph hostSt statesM rIndex [k,n,f,g]

-- Apply a match in a graph with paylads of type Info.
-- This function is auxiliar to applyMatch and it ensures that the payloads of nodes and edges of the final graph are correct.
applyMatchGraph :: [TGM.TypedGraphMorphism Info Info] -> G.Graph (Maybe Info) (Maybe Info) -> (G.Graph Info Info, G.Graph (Maybe Info) (Maybe Info), G.Graph (Maybe Info) (Maybe Info))
applyMatchGraph (k:n:f:g:_) gGraph = (finalGraph, dGraph, hGraph)
  where
    fMapping = TGM.mapping f
    gMapping = TGM.mapping g
    nMapping = TGM.mapping n
    fNodeRelation' = R.inverseRelation $ GM.nodeRelation fMapping -- G --fn'--> D
    fEdgeRelation' = R.inverseRelation $ GM.edgeRelation fMapping -- G --fe'--> D
    gNodeRelation = GM.nodeRelation gMapping                      -- D --gn--> H
    gEdgeRelation = GM.edgeRelation gMapping                      -- D --ge--> H

    -- auxiliar function
    replaceInfo (k,i) m = case lookup k m of Nothing -> i; Just i' -> i'

    -- make sure the mapped elements preserve the information between transformations
    dGraph = GM.codomainGraph fMapping
    hGraph = GM.codomainGraph gMapping

    gNodesInfo = map (\(k,n) -> (applyRelationWithDefault fNodeRelation' k (G.NodeId (-1)), G.nodeInfo n)) (G.nodeMap gGraph)
    gEdgesInfo = map (\(k,e) -> (applyRelationWithDefault fEdgeRelation' k (G.EdgeId (-1)), G.edgeInfo e)) (G.edgeMap gGraph)

    dNodesInfo = map (\(k,n) -> (k, G.nodeInfo n)) (G.nodeMap dGraph)
    dEdgesInfo = map (\(k,e) -> (k, G.edgeInfo e)) (G.edgeMap dGraph)
    dNodesInfo' = map (\(k,ni) -> (applyRelationWithDefault gNodeRelation k (G.NodeId (-1)), replaceInfo (k,ni) gNodesInfo) ) dNodesInfo
    dEdgesInfo' = map (\(k,ei) -> (applyRelationWithDefault gEdgeRelation k (G.EdgeId (-1)), replaceInfo (k,ei) gEdgesInfo) ) dEdgesInfo

    hNodesInfo = map (\(k,n) -> (k, replaceInfo (k, G.nodeInfo n) dNodesInfo')) (G.nodeMap hGraph)
    hEdgesInfo = map (\(k,e) -> (k, replaceInfo (k, G.edgeInfo e) dEdgesInfo')) (G.edgeMap hGraph)

    hGraph' = foldr (\(k,i) g -> G.updateNodePayload k g (\ni -> i)) hGraph hNodesInfo
    hGraph'' = foldr (\(k,i) g -> G.updateEdgePayload k g (\ei -> i)) hGraph' hEdgesInfo
    hNodeMap' = map (\(k,n) -> (k, GMker.nodeFromJust n)) (G.nodeMap hGraph'')
    hEdgeMap' = map (\(k,e) -> (k, GMker.edgeFromJust e)) (G.edgeMap hGraph'')
    finalNodeMap = map (\(k,n) -> (k, n {G.nodeInfo = infoSetOperation (G.nodeInfo n) Preserve})) hNodeMap'
    finalEdgeMap = map (\(k,e) -> (k, e {G.edgeInfo = infoSetOperation (G.edgeInfo e) Preserve})) hEdgeMap'
    finalGraph = G.Graph finalNodeMap finalEdgeMap

-- Change a graphState, updating it's graph and changing it's GI, positioning new nodes relatively to the position it has in the rule.
-- This function is an auxiliar to applyMatch.
applyMatchGI :: G.Graph Info Info -> G.Graph (Maybe Info) (Maybe Info) -> G.Graph (Maybe Info) (Maybe Info) ->  GraphState -> M.Map Int32 GraphState -> Int32 -> [TGM.TypedGraphMorphism Info Info] -> GraphState
applyMatchGI finalGraph dGraph hGraph hostSt statesM rIndex (k:n:f:g:_) = hState
  where
    fMapping = TGM.mapping f
    gMapping = TGM.mapping g
    nMapping = TGM.mapping n
    fNodeRelation' = R.inverseRelation $ GM.nodeRelation fMapping -- G --fn'--> D
    fEdgeRelation' = R.inverseRelation $ GM.edgeRelation fMapping -- G --fe'--> D
    gNodeRelation = GM.nodeRelation gMapping                      -- D --gn--> H
    gEdgeRelation = GM.edgeRelation gMapping                      -- D --ge--> H
    nNodeRelation = GM.nodeRelation nMapping                      -- R --nn--> H
    nNodeRelation' = R.inverseRelation $ GM.nodeRelation nMapping -- H --nn'--> R
    nEdgeRelation' = R.inverseRelation $ GM.edgeRelation nMapping -- H --ne'--> R



    (sgiN,sgiE) = stateGetGI hostSt
    -- delete layouts of elements that are not in the f morphism (G --f--> D)
    sgiN' = M.mapKeys G.NodeId $ M.filterWithKey (\k _ -> G.NodeId k `elem` (R.domain fNodeRelation')) sgiN
    sgiE' = M.mapKeys G.EdgeId $ M.filterWithKey (\k _ -> G.EdgeId k `elem` (R.domain fEdgeRelation')) sgiE
    -- modify the ids of the elements that are in the f morphism (G --f--> D)
    dgiN  = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> applyRelationWithDefault fNodeRelation' k (G.NodeId (-1))) sgiN'
    dgiE  = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> applyRelationWithDefault fEdgeRelation' k (G.EdgeId (-1))) sgiE'
    -- modify the ids of the elements that are in the g morphism (D --g--> H)
    dgiN' = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> applyRelationWithDefault gNodeRelation k (G.NodeId (-1))) dgiN
    dgiE' = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> applyRelationWithDefault gEdgeRelation k (G.EdgeId (-1))) dgiE

    -- add layouts to the elements of codomain of g morphism that have no relation to elements of it's domain
    rSt = fromJust $ M.lookup rIndex statesM
    (rgiN,rgiE) = stateGetGI rSt
    rGraph = stateGetGraph rSt
    addedNodeIds = filter (\id -> id `notElem` (M.keys dgiN')) (R.domain nNodeRelation')
    addedEdgeIds = filter (\id -> id `notElem` (M.keys dgiE')) (R.domain nEdgeRelation')
    addedNodeIds' = filter (\(_,kr) -> fromEnum kr > 0) $ map (\k -> (k, applyRelationWithDefault nNodeRelation' k (-1))) addedNodeIds
    addedEdgeIds' = filter (\(_,kr) -> fromEnum kr > 0) $ map (\k -> (k, applyRelationWithDefault nEdgeRelation' k (-1))) addedEdgeIds
    addedNodeGIs = map (\(k,kr) -> (k,fromJust $ M.lookup (fromEnum kr) rgiN)) addedNodeIds'
    addedEdgeGIs = map (\(k,kr) -> (k,fromJust $ M.lookup (fromEnum kr) rgiE)) addedEdgeIds'

    -- reposition added elements
    addedEdgeGIs' = calculateEdgesPositions addedEdgeIds addedEdgeGIs dGraph hGraph gNodeRelation
    addedNodeGIs' = calculateNodesPositions addedNodeIds' addedNodeGIs rGraph rgiN dgiN' nNodeRelation

    hgiN = foldr (\(k,gi) m -> M.insert k gi m) (M.mapKeys fromEnum $ dgiN') (map (\(k,gi) -> (fromEnum k, gi)) addedNodeGIs')
    hgiE = M.mapKeys fromEnum $ foldr (\(k,gi) m -> M.insert k gi m) dgiE' addedEdgeGIs'
    hState = stateSetSelected (addedNodeIds,addedEdgeIds) . stateSetGraph finalGraph . stateSetGI (hgiN,hgiE) $ hostSt



applyRelationWithDefault :: Ord a => R.Relation a -> a -> a -> a
applyRelationWithDefault rel k def =
  case R.apply rel k of
    [] -> def
    id:_ -> id



calculateEdgesPositions :: [G.EdgeId] -> [(G.EdgeId, EdgeGI)] -> G.Graph (Maybe Info) (Maybe Info) -> G.Graph (Maybe Info) (Maybe Info) -> R.Relation G.NodeId -> [(G.EdgeId, EdgeGI)]
calculateEdgesPositions addedEdgeIds addedEdgeGIs dGraph hGraph gNodeRelation =
    addedEdgeGIs'
    where
        -- 1. get src and tgt nodes from each edge in intermediary graph D;
        addedEdges = M.fromList $ map (\eid -> (eid, fromJust $ G.lookupEdge eid hGraph) ) addedEdgeIds
        gNodeRelation' = R.inverseRelation gNodeRelation
        addedEdgesPeerIds = M.map (\e -> (R.apply gNodeRelation' $ G.sourceId e, R.apply gNodeRelation' $ G.targetId e)) addedEdges
        -- 2. find what would be the added edges positions in D
        (_,edgesPositions) = M.foldrWithKey (\eid (srcl,tgtl) (g,m) ->  case (srcl,tgtl) of
                                                        (src:_,tgt:_)-> let pos = if src == tgt then newLoopPos src g else newEdgePos src tgt g
                                                                            newId = head $ G.newEdges g
                                                                            g' = G.insertEdge newId src tgt g
                                                                        in (g',M.insert eid pos m)
                                                        _ -> (g,m))
                            (dGraph,M.empty) addedEdgesPeerIds
        -- 3. update positions of the GIs
        addedEdgeGIs' = map (\(k,gi) -> case M.lookup k edgesPositions of
                                            Nothing -> (k,gi)
                                            Just p -> (k,gi {cPosition = p})) addedEdgeGIs


calculateNodesPositions :: [(G.NodeId,G.NodeId)] -> [(G.NodeId,NodeGI)] -> G.Graph Info Info -> M.Map Int NodeGI -> M.Map G.NodeId NodeGI  -> R.Relation G.NodeId -> [(G.NodeId,NodeGI)]
calculateNodesPositions addedNodeIds' addedNodeGIs rGraph rgiN dgiN' nNodeRelation = addedNodeGIs'
    where
         -- 1. find an anchor node foreach added node in R
        addedNodesInContext = map (\(k,kr) -> (k,kr,G.lookupNodeInContext kr rGraph)) addedNodeIds'
        addedNodesInContext' = map (\(k,kr,nc) -> (k,kr,fromJust nc) ) $ filter (\(_,_,nc) -> not $ null nc) addedNodesInContext
        nodeIsPreserved n = (infoOperation $ G.nodeInfo n) == Preserve
        preservedNodes = filter nodeIsPreserved (G.nodes rGraph)
        anchorNodesInR = map (\(k,kr,(n,c)) -> let  nextNodes = filter nodeIsPreserved $ map fst $ [tgt | (_,_,tgt) <- G.outgoingEdges c]
                                                    prevNodes = filter nodeIsPreserved $ map fst $ [src | (src,_,_) <- G.incomingEdges c]
                                                    anchorNodesIds = map G.nodeId $ case ( nextNodes ++ prevNodes, preservedNodes) of
                                                        (n1:n2:_,_) -> [n1,n2]
                                                        ([n],_) -> [n]
                                                        ([],n1:n2:_) -> [n1,n2]
                                                        ([],[n]) -> [n]
                                                        _ -> []
                                                in (k,kr,anchorNodesIds)
                                                ) addedNodesInContext'
        anchorNodesInR' = filter (\(k,kr,krA) -> not $ null krA) anchorNodesInR
        -- 2. calculate the relative position between each added node and it's anchor
        subPoint (a,b) (c,d) = (a-c,b-d)
        positionLists = map (\(k,kr,krAs) ->
                            let
                                posN = position $ getNodeGI (fromEnum kr) rgiN
                                posAs = map (\krA -> position $ getNodeGI (fromEnum krA) rgiN) krAs
                                posList = case posAs of
                                    [posA] -> [posN, posA]
                                    [posA1,posA2] -> [posN, posA1, posA2]
                                    _ -> []
                            in  (k,krAs,posList)
                            ) anchorNodesInR'
        -- 3. calculate position that the node should have in H
        anchorNodesInH = map (\(k,krAs,posF) -> (k,map (R.apply nNodeRelation) krAs,posF)) positionLists
        addedNodePositions = M.fromList
                                $ map (\(k,kAs,posL) -> let posAs = case kAs of
                                                                [(kA1:_),(kA2:_)] -> map (\kA -> position <$> M.lookup kA dgiN')  (map head kAs)
                                                                [kA:_] -> [position <$> M.lookup kA dgiN']
                                                                _ -> []
                                                            posN = case (posL,posAs) of
                                                                ([pN,pA],[Just pA']) -> Just $ addPoint pA' (subPoint pN pA)
                                                                ([pN,pA1,pA2],[Just pA1', Just pA2']) ->
                                                                                let angle1 = angle pA1 pA2
                                                                                    angle2 = angle pA1' pA2'
                                                                                    (a,d) = toPolarFrom pA1 pN
                                                                                    dist = d * ( vectorLength ( subPoint pA2' pA1' ) / vectorLength ( subPoint pA2 pA1 ) )
                                                                                in Just $ pointAt (a + (angle2 - angle1)) dist pA1'
                                                                _ -> Nothing
                                                        in (k,posN)
                                                        ) anchorNodesInH
        -- 4. add to GI
        addedNodeGIs' = map (\(k,gi) -> let newPos = M.lookup k addedNodePositions
                                            gi' = case newPos of
                                                    Just (Just pos) -> gi {position = pos}
                                                    _ -> gi
                                            gi'' = repositionNode gi' (M.mapKeys fromEnum dgiN',M.empty)
                                        in (k,gi'')
                                        ) addedNodeGIs
