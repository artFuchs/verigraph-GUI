{-|
Description : Syntax and model checking for LTL.

Maintainer  : Arthur Lucena Fuchs <alfuchs@inf.ufrgs.br>
Stability   : provisional
-}
module Logic.Ltl
  (
  -- * CTL Expressions
    module Logic.Ltl.Base
  , parseExpr

  -- * Model checking
  -- , check
  , module Logic.Ltl.Semantics
  ) where


import           Logic.Ltl.Base
import           Logic.Ltl.Parser
import           Logic.Ltl.Semantics
import           Logic.Model

import Control.Monad

example :: String -> IO ()
example formula = do
  let eExpr = parseExpr "" formula
  case eExpr of
    Left err -> putStrLn $ "Parsing error for formula " ++ formula ++ ":" ++ (show err)
    Right expr -> do
      let (initial,auto) = exprAutomaton expr
      putStrLn "states: "
      forM_ (states auto) print
      putStrLn $ "initial states: " ++ (show initial)
      putStrLn "transitions: "
      forM_ (transitions auto) print
      return ()


-- | Check if the given expression holds in the given state of the Kripke structure.
-- check :: KripkeStructure String -> Expr -> Int -> Bool
-- check model expr s0 = s0 `elem` satisfyExpr' model expr
