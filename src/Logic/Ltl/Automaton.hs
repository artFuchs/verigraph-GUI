module Logic.Ltl.Automaton (
  exprAutomaton
, Closure
, closure
, genStates
) where

import Logic.Ltl.Base
import qualified Logic.Model as Logic
import Data.Maybe (fromMaybe, catMaybes)
import Data.List ((\\))

import Data.Set (Set)
import qualified Data.Set as Set

type Closure = Set Expr

-- | given a LTL expression, generates an automaton for it
exprAutomaton :: Expr -> ([Int],Logic.KripkeStructure Expr)
exprAutomaton expr = (initial, Logic.KripkeStructure states transitions)
  where
    initial = map Logic.elementId $ filter isInitialState states
    transitions = genTransitions states
    states = genStates expr
    isInitialState :: Logic.State Expr -> Bool
    isInitialState s =
        case expr of
          (Literal b) -> b
          _ -> expr `elem` Logic.values s


genTransitions :: [Logic.State Expr] -> [Logic.Transition Expr]
genTransitions states = transitions
  where
    transitions = zipWith (\i (s,t) -> Logic.Transition i s t []) [0..] ts
    ts = genTransitions' states
    others state = states \\ [state]


genTransitions' :: [Logic.State Expr] -> [(Int, Int)]
genTransitions' states = map (\(a,b) -> (Logic.elementId a, Logic.elementId b)) selected
  where
    selected = filter satisfyRestrictions allPairs
    allPairs = [(a,b)| a <- states, b <- states]

    satisfyRestrictions :: (Logic.State Expr, Logic.State Expr) -> Bool
    satisfyRestrictions (Logic.State _ exprsA, Logic.State _ exprsB) =
      satisfyRestrictions' exprsA exprsB

    satisfyRestrictions' :: [Expr] -> [Expr] -> Bool
    satisfyRestrictions' exprsA exprsB = and $ map satisfy exprsA
      where
        satisfy (Temporal(X (Literal b))) = b
        satisfy (Temporal(X e)) = e `elem` exprsB
        satisfy (Not (Temporal (X (Literal b)))) = not b
        satisfy (Not (Temporal (X (Not e)))) = e `elem` exprsB
        satisfy (Not (Temporal (X e))) = (Not e) `elem` exprsB
        satisfy e@(Temporal (U (Literal b1) (Literal b2))) = b2 || (b1 && e `elem` exprsB)
        satisfy e@(Temporal (U (Literal b) e2)) = (b && e `elem` exprsB) || e2 `elem` exprsA
        satisfy e@(Temporal (U e1 (Literal b))) = b || e `elem` exprsB
        satisfy e@(Temporal (U e1 e2)) = e2 `elem` exprsA || e `elem` exprsB
        satisfy e@(Not (Temporal (U (Literal b1) (Literal b2)))) = not b2 && (not b1 || e `elem` exprsB)
        satisfy e@(Not (Temporal (U (Literal b) e2))) = not b || e `elem` exprsB
        satisfy e@(Not (Temporal (U e1 (Literal b)))) = e1 `notElem` exprsA || (not b && e `elem` exprsB)
        satisfy e@(Not (Temporal (U e1 e2))) = e1 `notElem` exprsA || e `elem` exprsB
        satisfy _ = True


genStates :: Expr -> [Logic.State Expr]
genStates expr =
  zipWith Logic.State [0..] (map Set.toList statesSets)
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
    respect (Implies (Literal b1) (Literal b2)) = not b1 || (b1 && b2)
    respect (Implies (Literal b) e2) = not b || (b && (e2 `Set.member` s))
    respect (Implies e1 e2) = ((e1 `Set.member` s) && (e2 `Set.member` s)) || (e1 `Set.notMember` s)
    respect (Equiv e1 e2) = (e1 `Set.member` s) == (e2 `Set.member` s)
    respect (Temporal (U (Literal False) (Literal False))) = False
    respect (Temporal (U e1 e2)) = (e1 `Set.member` s) || (e2 `Set.member` s)
    respect (Not (Temporal (U (Literal True) (Literal True)))) = False
    respect (Not (Temporal (U e1 e2))) = e2 `Set.notMember` s
    respect (Not e@(And e1 e2)) = not $ respect e
    respect (Not e@(Or e1 e2)) = not $ respect e
    respect (Not e@(Implies e1 e2)) = not $ respect e
    respect (Not e@(Equiv e1 e2)) = not $ respect e
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
