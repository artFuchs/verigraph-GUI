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
  -- putStrLn $ "Automaton for formula " ++ (show expr)
  -- forM_ (Logic.states (snd na)) print
  -- forM_ (Logic.transitions (snd na)) print
  -- putStrLn $ "initial states na: " ++ (show $ fst na)
  --
  -- putStrLn $ "combination"
  -- forM_ (Logic.states mXna) print
  -- forM_ (Logic.transitions mXna) print
  -- putStrLn $ "initial states mXna: " ++ (show $ initialSts)
  --
  -- putStrLn $ "path: " ++ (show path)
  -- putStrLn $ "path': " ++ (show path')

  -- return path'
  where
    path' = catMaybes $ map (\i -> IM.lookup i mapping) path
    path = findSatisfyingPath mXna initialSts

    -- combine model and automaton
    (initialSts, mXna, mapping) = combineModels (initial,model) na

    -- create expression automaton
    na = exprAutomaton expr'
    expr' = rewriteExpr (Not expr)


findSatisfyingPath :: Logic.KripkeStructure Expr -> [Int] -> [Int]
findSatisfyingPath model initialSts =
  case paths' of
    (p:_) -> p
    _ -> []
  where
    paths' = filter (\p -> pathSatisfyUConstraint model (reverse p)) paths
    paths = filter (not . null) $ concat $ map findPaths' initialSts
    findPaths' s = findPaths model [s]


findPaths :: Logic.KripkeStructure Expr -> [Int] -> [[Int]]
findPaths model path@(i:is) =
  case nextStates of
    [] -> []
    ns -> concat
          $ map (\n ->
                if n `elem` path
                  then [n:path]
                  else findPaths model (n:path)) ns
  where
    nextStates = Logic.nextStates model i

pathSatisfyUConstraint model path@(i:is) =
  and $ map pathSatisfyU us
  where
    exprs s = Logic.values $ Logic.getState s model
    us = filter (\e -> case e of
                      (Temporal (U a b)) -> True
                      _ -> False)
                (exprs i)
    pathSatisfyU u@(Temporal(U a b)) =
      or $
        map (\s -> let es = (exprs s)
                      in b == (Literal True)
                      || b `elem` es
                      || (Not u) `elem` es)
        is
