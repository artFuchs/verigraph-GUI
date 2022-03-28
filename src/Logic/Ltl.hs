{-|
Description : Syntax and model checking for LTL.

Maintainer  : Arthur Lucena Fuchs <alfuchs@inf.ufrgs.br>
Stability   : provisional
-}
module Logic.Ltl
  (
  -- * LTL Expressions
    module Logic.Ltl.Base
  , parseExpr

  -- * Model checking
  -- , check
  , module Logic.Ltl.Semantics
  , module Logic.Ltl.Automaton
  ) where

import           Logic.Ltl.Automaton

import           Logic.Ltl.Base
import           Logic.Ltl.Parser
import           Logic.Ltl.Semantics
import           Logic.Model

import Control.Monad

-- | Check if the given expression holds in the given state of the Kripke structure.
-- check :: KripkeStructure String -> Expr -> Int -> Bool
-- check model expr s0 = length (satisfyExpr model expr) > 0
