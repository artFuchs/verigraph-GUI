{-# LANGUAGE ScopedTypeVariables        #-}

-- | This module handles state space generation
module GUI.Analysis.ModelChecker.StateSpace (
  exploreStateSpace
, breadthFirstSearchIO
, NamedPredicate
, NamedProduction
, Space
) where

import            Control.Monad
import            Control.Concurrent.MVar

import qualified  Abstract.Category                 as Cat
import qualified  Abstract.Rewriting.DPO            as DPO
import qualified  Abstract.Rewriting.DPO.StateSpace as SS
import qualified  Data.Graphs                       as G
import qualified  Data.TypedGraph.Morphism          as TGM
import qualified  Data.TypedGraph                   as TG
import            Rewriting.DPO.TypedGraph


type NamedPredicate a b = (String, TypedGraphRule a b)
type NamedProduction a b = (String, TypedGraphRule a b)
type Space a b = SS.StateSpace (TGM.TypedGraphMorphism a b)


-- state space generation -------------------------------------------------------------------------------------------------------------------------------------------------------
exploreStateSpace :: DPO.MorphismsConfig (TGM.TypedGraphMorphism a b) -> Int -> DPO.Grammar (TGM.TypedGraphMorphism a b) -> TG.TypedGraph a b -> Maybe (MVar (Space a b, Bool)) -> IO (Int,Space a b)
exploreStateSpace conf maxDepth grammar graph ssMVar =
  {-# SCC "exploreStateSpace" #-}
  let
    (productions, predicates) =
      splitPredicates (DPO.productions grammar)

    getInitialId =
      do
        (idx, _) <- SS.putState graph
        return idx

    initialSpace =
      SS.empty conf productions predicates
  in do
    (idx,space1) <- return $ SS.runStateSpaceBuilder getInitialId initialSpace
    spacex <- breadthFirstSearchIO maxDepth [graph] space1 ssMVar
    return (idx,spacex)


{-| Runs a breadth-first search, starting with a initial state space.
    The number of states explored are limited by the given number.
    At each iteration of the search, updates the MVar ssMVar with the current result so that the user can stop at any moment.
-}
breadthFirstSearchIO :: Int -> [(TG.TypedGraph a b)] -> Space a b -> Maybe (MVar (Space a b, Bool)) -> IO (Space a b)
breadthFirstSearchIO 0 _ initialSpace mssMVar = return initialSpace
breadthFirstSearchIO _ [] initialSpace mssMVar = return initialSpace
breadthFirstSearchIO maxNum objs initialSpace mssMVar = do
  let ((newNum, newObjs), state) = SS.runStateSpaceBuilder (bfsStep maxNum objs) initialSpace
  case mssMVar of
    Just ssMVar -> putMVar ssMVar (state, null newObjs || newNum == 0)
    Nothing -> return ()
  breadthFirstSearchIO newNum newObjs state mssMVar


-- | Step of a breadth-first search.
bfsStep :: forall morph. DPO.DPO morph => Int -> [Cat.Obj morph] -> SS.StateSpaceBuilder morph (Int,[Cat.Obj morph])
bfsStep maxNum (obj:node_list) = do
  (objIndex, _) <- SS.putState obj
  successors <- expandSuccessors' maxNum (objIndex,obj)
  let successors' = map (\(i,o,n) -> o) successors
  return (maxNum - length successors', node_list ++ successors')


-- | Finds all transformations of the given state with the productions of the HLR system being explored, adding them to the state space.
-- Returns a list of the successor states as @(index, object, isNew)@, where @isNew@ indicates that the state was not present in the state space before.
-- limits the number of matches applications to @maxNum@
-- adapted from module Abstract.Rewriting.DPO.StateSpace
expandSuccessors' :: forall morph. DPO.DPO morph => Int -> (Int, Cat.Obj morph) -> SS.StateSpaceBuilder morph [(Int, Cat.Obj morph, Bool)]
expandSuccessors' maxNum (index, object) =
  do
    prods <- SS.getProductions
    conf <- SS.getDpoConfig
    matches <- return . concat . map (\(name,prod) -> map (\match -> (name,prod,match)) (DPO.findApplicableMatches conf prod object)) $ prods
    (_,states) <- foldM expand (maxNum, []) matches
    return states
  where
    expand :: (Int, [(Int, Cat.Obj morph, Bool)]) -> (String, Production morph, morph) -> SS.StateSpaceBuilder morph  (Int, [(Int, Cat.Obj morph, Bool)])
    expand (n,l) (name,prod,match) =
      if (n > 0) then
        do
          let object' = DPO.rewrite match prod
          (index', isNew) <- SS.putState object'
          SS.putTransition (index,index',name)
          return $
            if isNew then
              (n-1,(index',object',isNew):l)
            else
              (n,l)
      else
        return (0,l)




-- | Separates the rules that change nothing (which are considered predicates)
-- from those that have some effect (which are considered productions).
-- definition taken as is from Verigraph CLI/ModelChecker.hs
splitPredicates :: [(String, TypedGraphRule a b)] -> ([NamedProduction a b], [NamedPredicate a b])
splitPredicates [] =
  ([], [])

splitPredicates ((name, rule) : rest) =
  let
    (productions, predicates) =
      splitPredicates rest
  in
    if Cat.isIsomorphism (leftMorphism rule) && Cat.isIsomorphism (rightMorphism rule) then
      (productions, (name, rule):predicates)
    else
      ((name, rule):productions, predicates)
