module Logic.Ltl.Combine (
  combineModels
) where


import Logic.Ltl.Base
import qualified Logic.Model as Logic
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM


-- | Given a model and a automaton for a LTL expression, combine the two,
combineModels :: ([Int],Logic.KripkeStructure String) -> ([Int],Logic.KripkeStructure Expr) -> ([Int], Logic.KripkeStructure Expr, IntMap Int)
combineModels (initialSts,model) (initialSts',autom) =
  (initialSts'', Logic.KripkeStructure states transitions, statesMapping)
  where
    statesMapping = IM.map fst statesPairs
    initialSts'' = IM.keys $ IM.filter (\(s,k) -> s `elem` initialSts && k `elem` initialSts') statesPairs
    transitions = joinTransitions model autom statesPairs
    states = IM.elems $ IM.mapWithKey (\i (s,k) -> Logic.State i (Logic.values . Logic.getState k $ autom)) statesPairs
    statesPairs = createAtoms (Logic.states model) (Logic.states autom)


joinTransitions :: Logic.KripkeStructure String -> Logic.KripkeStructure Expr -> IntMap (Int, Int) -> [Logic.Transition Expr]
joinTransitions model autom statesPairs =
  zipWith (\i (s,t) -> Logic.Transition i s t []) [0..] atomstransitions
  where
    atomstransitions = concat $ IM.elems $ IM.mapWithKey (\k a -> zip (repeat k) (atomTransitions k a)) statesPairs
    atomTransitions i a = IM.keys $ IM.filter (hasTransition a) statesPairs

    hasTransition (s1,k1) (s2,k2) =
      let
        ts = stateTransitions s1 model
        tk = stateTransitions k1 autom
      in
        s2 `elem` (map Logic.target ts) && k2 `elem` (map Logic.target tk)

    stateTransitions s ks = filter (\t -> Logic.source t == s) $ Logic.transitions ks

-- create Atoms, pairs of compatible states between a model and a automaton
createAtoms :: [Logic.State String] -> [Logic.State Expr] -> IntMap (Int, Int)
createAtoms modelStates automatonStates = IM.fromList $ zip [0..] compatiblePairs
  where
    compatiblePairs = concat $ map (\s -> zip (repeat $ Logic.elementId s) (getCompatibleASts s)) modelStates
    getCompatibleASts s = map Logic.elementId $ filter (statesAreCompatible s) automatonStates
    statesAreCompatible (Logic.State _ ps) (Logic.State _ exprs) = containsAtoms && dontContainNegativeAtoms
      where
        containsAtoms = and $ map (\p -> p `elem` ps) (extractAtoms exprs)
        dontContainNegativeAtoms = and $ map (\p -> p `notElem` ps) (extractNegativeAtoms exprs)

        extractAtoms :: [Expr] -> [String]
        extractAtoms [] = []
        extractAtoms ((Atom p):ps) = p : (extractAtoms ps)
        extractAtoms (_:ps) = extractAtoms ps

        extractNegativeAtoms :: [Expr] -> [String]
        extractNegativeAtoms [] = []
        extractNegativeAtoms ((Not(Atom p)):ps) = p : (extractNegativeAtoms ps)
        extractNegativeAtoms (_:ps) = extractNegativeAtoms ps
