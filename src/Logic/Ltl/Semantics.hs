module Logic.Ltl.Semantics (
  satisfyExpr
) where

import Logic.Ltl.Base
import Logic.Ltl.Parser
import Logic.Ltl.Automaton
import Logic.Ltl.Combine

import qualified Logic.Model as Logic

import Control.Monad
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.IntMap as IM




-- | Checks if there is a path that satisfies 'Not expr'.
-- If the returned result is [] then the expression passed holds for the system
-- else the returned path (in reverse) is a counter example for the expression
satisfyExpr :: Logic.KripkeStructure String -> [Int] -> Expr -> [Int]
satisfyExpr model initial expr = path'
  where
    path' = catMaybes $ map (\i -> IM.lookup i mapping) path
    path = findSatisfyingPath mXna expr' initialSts

    -- combine model and automaton
    (initialSts, mXna, mapping) = combineModels (initial,model) na

    -- create expression automaton
    na = exprAutomaton expr'
    expr' = rewriteExpr (Not expr)


findSatisfyingPath :: Logic.KripkeStructure Expr -> Expr -> [Int] -> [Int]
findSatisfyingPath model expr initialSts =
  case paths of
    (p:_) -> p
    _ -> []
  where
    paths = filter (not . null) $ concat $ map findPaths initialSts
    findPaths s = findSatisfyingPaths model expr [s]


findSatisfyingPaths :: Logic.KripkeStructure Expr -> Expr -> [Int] -> [[Int]]
findSatisfyingPaths model (Literal False) path@(i:is) = []

findSatisfyingPaths model t@(Literal True) path@(i:is) =
  case nextStates'' of
    [] -> []
    ns -> concat $ map (\n -> if n `elem` path then [n:path] else findSatisfyingPaths model t (n:path)) ns
  where
    nextStates = Logic.nextStates model i
    nextStates' = filter (`elem` path) nextStates
    nextStates'' = if null nextStates' then nextStates else nextStates'

findSatisfyingPaths model e@(Atom a) path@(i:is) =
  if e `elem` exprs
    then findSatisfyingPaths model (Literal True) path
    else []
  where
    exprs = Logic.values $ Logic.getState i model


findSatisfyingPaths model ne@(Not e) path@(i:is) =
  if ne `elem` exprs || e `notElem` exprs
    then findSatisfyingPaths model (Literal True) path
    else []
  where
    exprs = Logic.values $ Logic.getState i model

findSatisfyingPaths model xe@(Temporal (X e)) path@(i:is) =
  nextPaths
  where
    nextStates = Logic.nextStates model i
    nextPaths = filter (not . null) $ concat $ map (\n -> findSatisfyingPaths model e (n:path)) nextStates

findSatisfyingPaths model e@(Temporal (U a b)) path@(i:is) =
  case (e `elem` exprs, b `elem` exprs) of
    (True, True) -> map (++is) $ findSatisfyingPaths model b [i]
    (True, False) -> nextPaths
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    nextStates = filter (`notElem` path) $ Logic.nextStates model i
    nextPaths = filter (not . null) $ concat $ map (\n -> findSatisfyingPaths model e (n:path)) nextStates

findSatisfyingPaths model e@(And a b) path@(i:is) =
  case (e `elem` exprs, null pathA || null pathB) of
    (True, False) -> findSatisfyingPaths model (Literal True) path
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    pathA = findSatisfyingPath model a [i]
    pathB = findSatisfyingPath model b [i]

findSatisfyingPaths model e@(Or a b) path@(i:is) =
  case (e `elem` exprs, null pathA && null pathB) of
    (True, False) -> findSatisfyingPaths model (Literal True) path
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    pathA = findSatisfyingPath model a [i]
    pathB = findSatisfyingPath model b [i]

findSatisfyingPaths model e@(Implies a b) path@(i:is) =
  case (e `elem` exprs, pathA) of
    (True, a:as) -> [pathB ++ is]
    (True, []) -> findSatisfyingPaths model (Literal True) path
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    pathA = findSatisfyingPath model a [i]
    pathB = findSatisfyingPath model b [i]

findSatisfyingPaths model e@(Equiv a b) path@(i:is) =
  case (e `elem` exprs, pathA, pathB) of
    (True, a:as, b:bs) -> findSatisfyingPaths model (Literal True) path
    (True, [], []) -> findSatisfyingPaths model (Literal True) path
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    pathA = findSatisfyingPath model a [i]
    pathB = findSatisfyingPath model b [i]
