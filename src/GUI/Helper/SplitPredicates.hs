module GUI.Helper.SplitPredicates(
  NamedPredicate
, NamedProduction
, splitPredicates
, isPredicate
) where

import qualified  Abstract.Category                 as Cat
import            Rewriting.DPO.TypedGraph

type NamedPredicate a b = (String, TypedGraphRule a b)
type NamedProduction a b = (String, TypedGraphRule a b)


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
    if isPredicate rule then
      (productions, (name, rule):predicates)
    else
      ((name, rule):productions, predicates)


isPredicate :: TypedGraphRule a b -> Bool
isPredicate rule = Cat.isIsomorphism (leftMorphism rule) && Cat.isIsomorphism (rightMorphism rule)
