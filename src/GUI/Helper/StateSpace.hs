module GUI.Helper.StateSpace (
  generateStateSpace
) where

import qualified Data.IntMap                        as IntMap

import qualified  Abstract.Category                 as Cat
import qualified  Abstract.Rewriting.DPO            as DPO
import qualified  Abstract.Rewriting.DPO.StateSpace as SS
import qualified  Data.TypedGraph.Morphism          as TGM
import qualified  Data.TypedGraph                   as TG
import            Rewriting.DPO.TypedGraph

import            GUI.Helper.GrammarMaker
import            GUI.Data.Info

generateStateSpace :: DPO.Grammar (TGM.TypedGraphMorphism Info Info) -> IO ()
generateStateSpace grammar = do
  let initialGraph = DPO.start grammar
      mconf = (DPO.MorphismsConfig Cat.monic) :: DPO.MorphismsConfig (TGM.TypedGraphMorphism Info Info)
      (initialState, stateSpace) = exploreStateSpace mconf 5 grammar [("initialGraph",initialGraph)]
  putStrLn $ "spaceLength: " ++ (show $ IntMap.size $ SS.states stateSpace)
  return ()




-- definitions taken from Verigraph CLI/ModelChecker.hs
exploreStateSpace :: DPO.MorphismsConfig (TGM.TypedGraphMorphism a b) -> Int -> DPO.Grammar (TGM.TypedGraphMorphism a b) -> [(String, TG.TypedGraph a b)] -> ([Int], Space a b)
exploreStateSpace conf maxDepth grammar graphs =
  let
    (productions, predicates) =
      splitPredicates (DPO.productions grammar)

    searchFrom (_, graph) =
      do
        (idx, _) <- SS.putState graph
        SS.depthSearch maxDepth graph
        return idx

    search =
      mapM searchFrom graphs

    initialSpace =
      SS.empty conf (map snd productions) predicates
  in
    SS.runStateSpaceBuilder search initialSpace

type NamedPredicate a b = (String, TypedGraphRule a b)
type NamedProduction a b = (String, TypedGraphRule a b)
type Space a b = SS.StateSpace (TGM.TypedGraphMorphism a b)

-- | Separates the rules that change nothing (which are considered predicates)
-- from those that have some effect (which are considered productions).
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
