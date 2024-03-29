{-# LANGUAGE ScopedTypeVariables        #-}

-- | This module handles the state space generation and visualization.
module GUI.Analysis.ModelChecker.StateSpace (
  exploreStateSpace
, breadthFirstSearchIO
, generateStateSpaceVisualization
, NamedPredicate
, NamedProduction
, Match
, Space
) where

import            Control.Monad
import            Control.Concurrent.MVar
import            Data.Maybe
import qualified  Data.Map                          as M
import            Data.IntMap                       (IntMap)
import qualified  Data.IntMap                       as IntMap

import qualified  Abstract.Category                 as Cat
import qualified  Abstract.Rewriting.DPO            as DPO
import qualified  Abstract.Rewriting.DPO.StateSpace as SS
import qualified  Data.Graphs                       as G
import qualified  Data.TypedGraph.Morphism          as TGM
import qualified  Data.TypedGraph                   as TG
import            Rewriting.DPO.TypedGraph
import            GUI.Data.GraphState
import            GUI.Data.Info                     hiding (empty)
import qualified  GUI.Data.Info                     as Info
import            GUI.Data.GraphicalInfo
import            GUI.Helper.SplitPredicates



type Match a b = TGM.TypedGraphMorphism a b
type Space a b = SS.StateSpace (Match a b)


-- state space generation -------------------------------------------------------------------------------------------------------------------------------------------------------
{-| Explore the state space, starting from the initial graph.
    Returns a tuple @(initial index, state space, matchesMap)@ where
      - for now the @initial index@ is always 0
      - matchesMap is a IntMap containing a tuple @(i,m,n,p)@ where:
        - the key is the index of a generated state S
        - @i@ is the index of the state O that was used as base to generate S
        - @m@ is the match found between the lhs of the prodution and O
        - @p@ is the predution applied to generate S
-}
exploreStateSpace :: DPO.MorphismsConfig (TGM.TypedGraphMorphism a b) -> Int -> DPO.Grammar (TGM.TypedGraphMorphism a b) -> TG.TypedGraph a b -> Maybe (MVar (Space a b, Bool)) -> IO (Int,Space a b,IntMap (Int, Match a b, String, TypedGraphRule a b), Bool)
exploreStateSpace conf maxDepth grammar graph ssMVar =
  {-# SCC "exploreStateSpace" #-}
  let (productions, predicates) = splitPredicates (DPO.productions grammar)
      initialSpace = SS.empty conf productions predicates
      getInitialId = do
        (idx, _) <- SS.putState graph
        return idx
  in do
    (idx,space1) <- return $ SS.runStateSpaceBuilder getInitialId initialSpace
    (spacex, matchesM, completed) <- breadthFirstSearchIO maxDepth [graph] (IntMap.empty) space1 ssMVar
    return (idx, spacex, matchesM, completed)


{-| Runs a breadth-first search, starting with a initial state space.
    The number of states explored are limited by the given number.
    At each iteration of the search, updates the MVar ssMVar with the current result so that the user can stop at any moment.
    Returns a tuple @(state space, matchesMap, completed)@
-}
breadthFirstSearchIO :: Int -> [(TG.TypedGraph a b)] -> IntMap (Int, Match a b, String, TypedGraphRule a b) -> Space a b -> Maybe (MVar (Space a b, Bool)) -> IO (Space a b, IntMap (Int, Match a b, String, TypedGraphRule a b),Bool)
breadthFirstSearchIO 0 objs matchesM initialSpace _ = return (initialSpace, matchesM, (length objs) == 0)
breadthFirstSearchIO _ [] matchesM initialSpace _ = return (initialSpace, matchesM, True)
breadthFirstSearchIO maxNum objs matchesM initialSpace mssMVar = do
  let
    ((newNum, newObjs, matchesM'), state) = SS.runStateSpaceBuilder (bfsStep maxNum objs) initialSpace
    matchesM'' = IntMap.union matchesM matchesM'
  case mssMVar of
    Just ssMVar -> putMVar ssMVar (state, null newObjs || newNum == 0)
    Nothing -> return ()
  breadthFirstSearchIO newNum newObjs matchesM'' state mssMVar


-- | Step of a breadth-first search.
-- Returns a triple @(number of states to explore, explored states, matchesMap)@
-- where @matchesMap@ is a IntMap containing a triple @(prev index, state match, production)@ and the key is the index of the state that is generated by the match.
bfsStep :: forall morph. DPO.DPO morph => Int -> [Cat.Obj morph] -> SS.StateSpaceBuilder morph (Int,[Cat.Obj morph], IntMap (Int, morph, String, Production morph))
bfsStep maxNum (obj:node_list) = do
  (objIndex, _) <- SS.putState obj
  successors <- expandSuccessors' maxNum (objIndex,obj)
  let successors' = map (\(i,o,m) -> o) successors
  let matchesM = IntMap.fromList $ map (\(i,o,m) -> (i,m)) successors
  return (maxNum - length successors', node_list ++ successors', matchesM)


-- | Finds all transformations of the given state with the productions of the HLR system being explored, adding them to the state space.
-- Returns a list of the successor states as @(index, object, (new index, applied match, prodection))@.
-- Limits the number of matches applications to @maxNum@.
-- Adapted from module Abstract.Rewriting.DPO.StateSpace.
expandSuccessors' :: forall morph. DPO.DPO morph => Int -> (Int, Cat.Obj morph) -> SS.StateSpaceBuilder morph [(Int, Cat.Obj morph, (Int, morph, String, Production morph))]
expandSuccessors' maxNum (index, object) =
  do
    prods <- SS.getProductions
    conf <- SS.getDpoConfig
    matches <- return . concat . map (\(name,prod) -> map (\match -> (name,prod,match)) (DPO.findApplicableMatches conf prod object)) $ prods
    (_,states) <- foldM expand (maxNum, []) matches
    return states
  where
    expand :: (Int, [(Int, Cat.Obj morph, (Int, morph, String, Production morph))]) -> (String, Production morph, morph) -> SS.StateSpaceBuilder morph  (Int, [(Int, Cat.Obj morph, (Int, morph, String, Production morph))])
    expand (n,l) (name,prod,match) =
      if (n > 0) then
        do
          let object' = DPO.rewrite match prod
          (index', isNew) <- SS.putState object'
          SS.putTransition (index,index',name)
          return $
            if isNew then
              (n-1,(index',object', (index,match,name,prod)):l)
            else
              (n,l)
      else
        return (0,l)




-- | Given a state space and a graphstate, update the graphstate to 
generateStateSpaceVisualization :: Space Info Info -> GraphState -> GraphState
generateStateSpaceVisualization stateSpace st = st''
  where
    addLevel levels (a,b) = case M.lookup a levels of
                              Nothing -> M.insert b 1 $ M.insert a 0 levels
                              Just l -> M.alter
                                        (\mx -> case mx of
                                                  Nothing -> Just (l+1)
                                                  Just x -> Just (min x (l+1)))
                                        b levels
    nidsWithLevels = foldl addLevel (M.singleton 0 0) $ M.keys (SS.transitions stateSpace) :: M.Map Int Int
    organizeLevels a l levels = case M.lookup l levels of
                              Nothing -> M.insert l [a] levels
                              Just ls -> M.insert l (ls++[a]) levels
    levels = M.foldrWithKey organizeLevels (M.empty) nidsWithLevels :: M.Map Int [Int]
    getStatePredicates nid = fromMaybe [] $ snd <$> (IntMap.lookup nid (SS.states stateSpace))
    nodeWithPos nid posX posY =
      let
        predicates = getStatePredicates nid
        label = case predicates of
                  [] -> ("state " ++ show nid)
                  ps -> init . unlines $ ("state " ++ show nid ++ "\n"):predicates
      in
        (G.Node (G.NodeId nid) (infoSetLabel Info.empty label), (fromIntegral posX, fromIntegral posY))
    addNodeInLevel l nids nds = foldr (\(nid,posX) ls -> (nodeWithPos nid posX (l*100)):ls ) nds (zip nids [0,100..])
    nds = M.foldrWithKey addNodeInLevel [] levels
    ndsGIs = M.fromList $ map (\(n,pos) -> (fromEnum $ G.nodeId n, newNodeGI {position = pos, shape = NRect})) nds
    g = G.fromNodesAndEdges (map fst nds) []
    st' = stateSetGI (ndsGIs,M.empty) $ stateSetGraph g $ st
    st'' = M.foldrWithKey (\(a,b) names st -> createEdge st Nothing (G.NodeId a) (G.NodeId b) (infoSetLabel Info.empty (init $ unlines names)) False ENormal (0,0,0)) st' (SS.transitions stateSpace)
