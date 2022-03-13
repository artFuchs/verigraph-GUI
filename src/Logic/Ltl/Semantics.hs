module Logic.Ltl.Semantics (
  rewriteExpr
, closure
, Closure
, exprAutomaton
, genTransitions
, genStateIdPairs
, genStatesSets
, clearLiterals
, modelXautomaton
, findCompatibleStates
, joinTransitions
, extractAtoms
) where

import Logic.Ltl.Base
import Logic.Ltl.Parser
import qualified Logic.Model as Logic

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List ((\\), nub)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM


type Closure = Set Expr


-- | Given a automaton
modelXautomaton :: Logic.KripkeStructure String -> ([Int],Logic.KripkeStructure Expr) -> ([Int], Logic.KripkeStructure Expr)
modelXautomaton model (initialSts,autom) =
  (initialSts', Logic.KripkeStructure states transitions)
  where
    initialSts' = IM.keys $ IM.filter (\(s,k) -> s == 0 && k `elem` initialSts) statesPairs
    transitions = joinTransitions model autom statesPairs
    states = IM.elems $ IM.mapWithKey (\i (s,k) -> Logic.State i (Logic.values . Logic.getState k $ autom)) statesPairs
    statesPairs = findCompatibleStates (Logic.states model) (Logic.states autom)


joinTransitions :: Logic.KripkeStructure String -> Logic.KripkeStructure Expr -> IntMap (Int, Int) -> [Logic.Transition Expr]
joinTransitions model autom statesPairs =
  zipWith (\i (s,t) -> Logic.Transition i s t []) [1..] atomstransitions
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


findCompatibleStates :: [Logic.State String] -> [Logic.State Expr] -> IntMap (Int, Int)
findCompatibleStates modelStates automatonStates = IM.fromList $ zip [1..] compatiblePairs
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


-- | given a expression, generates an automaton for it
exprAutomaton :: Expr -> ([Int],Logic.KripkeStructure Expr)
exprAutomaton expr = (initial, Logic.KripkeStructure states transitions)
  where
    initial = map Logic.elementId $ filter (\s -> expr `elem` Logic.values s) states
    transitions = genTransitions states
    states = genStates expr


genTransitions :: [Logic.State Expr] -> [Logic.Transition Expr]
genTransitions states = transitions
  where
    transitions = zipWith (\i (s,t) -> Logic.Transition i s t []) [1..] idPairs
    idPairs = genStateIdPairs states
    others state = states \\ [state]


genStateIdPairs :: [Logic.State Expr] -> [(Int, Int)]
genStateIdPairs states = map (\(a,b) -> (Logic.elementId a, Logic.elementId b)) selected
  where
    selected = filter satisfyRestrictions allPairs
    allPairs = [(a,b)| a <- states, b <- states]

    satisfyRestrictions :: (Logic.State Expr, Logic.State Expr) -> Bool
    satisfyRestrictions (Logic.State _ exprsA, Logic.State _ exprsB) =
      transitionSatisfy exprsA exprsB

    transitionSatisfy exprsA exprsB = and $ map satisfy exprsA
      where
        satisfy (Temporal(X (Literal b))) = b
        satisfy (Temporal(X e)) = e `elem` exprsB
        satisfy (Not (Temporal (X (Literal b)))) = not b
        satisfy (Not (Temporal (X e))) = (Not e) `elem` exprsB
        satisfy e@(Temporal (U e1 (Literal b))) = b || e `elem` exprsB
        satisfy e@(Temporal (U e1 e2)) = e2 `elem` exprsA || e `elem` exprsB
        satisfy e@(Not (Temporal (U (Literal b) e2))) = not b || e `elem` exprsB
        satisfy e@(Not (Temporal (U e1 e2))) = e1 `notElem` exprsA || e `elem` exprsB
        satisfy _ = True


genStates :: Expr -> [Logic.State Expr]
genStates expr =
  zipWith Logic.State [1..] (map Set.toList statesSets)
  where
    statesSets = genStatesSets (closure expr)

genStatesSets :: Closure -> [Set Expr]
genStatesSets clos = Set.toList clearedSets
  where
    clearedSets = Set.map clearLiterals respectingsets
    respectingsets = Set.filter respectOps maxsubsets
    maxsubsets = Set.filter (\s -> Set.size s == maxSize) consistentsets
    maxSize = fromMaybe 0 $ Set.lookupMax (Set.map Set.size consistentsets)
    consistentsets = Set.filter isConsistent powerset
    powerset = Set.powerSet clos


clearLiterals :: Set Expr -> Set Expr
clearLiterals set = Set.filter notLiteral set
  where
    notLiteral (Literal _) = False
    notLiteral _ = True



respectOps :: Set Expr -> Bool
respectOps s = and . Set.toList $ Set.map respect s
  where
    respect (And e1 e2) = (e1 `Set.member` s) && (e2 `Set.member` s)
    respect (Or e1 e2) = (e1 `Set.member` s) || (e2 `Set.member` s)
    respect (Implies e1 e2) = ((e1 `Set.member` s) && (e2 `Set.member` s)) || (e1 `Set.notMember` s)
    respect (Equiv e1 e2) = ((e1 `Set.member` s) && (e2 `Set.member` s)) ||
                             ((e1 `Set.notMember` s) && (e2 `Set.notMember` s))
    respect (Temporal (U e1 e2)) = respect (Or e1 e2)
    respect (Not (Temporal (U e1 e2))) = (Not e2) `Set.member` s
    respect _ = True

isConsistent :: Set Expr -> Bool
isConsistent s = and . Set.toList $ Set.map hasNoOpposite s
  where
    hasNoOpposite (Literal b) = Literal (not b) `Set.notMember` s
    hasNoOpposite (Not e) = e `Set.notMember` s
    hasNoOpposite e = (Not e) `Set.notMember` s



-- | Given an expression, rewrite it in terms of X and U and obtain it's closure.
-- The closure cantains all subformulas of the expression and theirs negations,
-- and it identifies Not(Not( E )) and E.
closure :: Expr -> Closure
closure expr = closure' (rewriteExpr expr)

closure' :: Expr -> Closure
closure' (Not e) = closure e

closure' expr@(Implies e1 e2) =
  (Set.fromList [expr, Not expr]) `Set.union` closure e1 `Set.union` closure e2

closure' expr@(Equiv e1 e2) =
  (Set.fromList [expr, Not expr]) `Set.union` closure e1 `Set.union` closure e2

closure' expr@(And e1 e2) =
  (Set.fromList [expr, Not expr]) `Set.union` closure e1 `Set.union` closure e2

closure' expr@(Or e1 e2) =
  (Set.fromList [expr, Not expr]) `Set.union` closure e1 `Set.union` closure e2

closure' expr@(Temporal(U e1 e2)) =
  (Set.fromList [expr, Not expr]) `Set.union` closure e1 `Set.union` closure e2

closure' expr@(Temporal(X e)) =
  (Set.fromList [expr, Not expr]) `Set.union` closure e

closure' (Literal _) = Set.fromList  [Literal True, Literal False]

closure' e = Set.fromList [e, Not e]


-- rewrite expression in terms of X and U
rewriteExpr :: Expr -> Expr
rewriteExpr (Not (Literal True)) = Literal False
rewriteExpr (Not (Literal False)) = Literal True

rewriteExpr (Not e) =
  Not (rewriteExpr e)

rewriteExpr (And e1 e2) =
  And (rewriteExpr e1) (rewriteExpr e2)

rewriteExpr (Or e1 e2) =
  Or (rewriteExpr e1) (rewriteExpr e2)

rewriteExpr (Implies e1 e2) =
  Implies (rewriteExpr e1) (rewriteExpr e2)

rewriteExpr (Equiv e1 e2) =
  Equiv (rewriteExpr e1) (rewriteExpr e2)

rewriteExpr (Temporal (X e)) =
  Temporal (X (rewriteExpr e))

rewriteExpr (Temporal (F e)) =
  Temporal (U (Literal True) (rewriteExpr e))

rewriteExpr (Temporal (G e)) =
  rewriteExpr (Temporal (R (Literal False) e))

rewriteExpr (Temporal (U e1 e2)) =
  Temporal (U (rewriteExpr e1) (rewriteExpr e2))

rewriteExpr (Temporal (W e1 e2)) =
  Or
    (Temporal (U (rewriteExpr e1) (rewriteExpr e2)))
    (rewriteExpr (Temporal (G e1)))

rewriteExpr (Temporal (R e1 e2)) =
  Not (Temporal (U (rewriteExpr $ Not e1) (rewriteExpr $ Not e2)))

rewriteExpr e = e
