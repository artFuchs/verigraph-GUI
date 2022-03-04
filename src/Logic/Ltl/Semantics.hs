module Logic.Ltl.Semantics (
  rewriteExpr
, closure
) where

import Logic.Ltl.Base
import Logic.Ltl.Parser

import Data.List

type Closure a = [a]



-- | Given an expression, rewrite it in terms of X and U and obtain it's closure.
-- The closure cantains all subformulas of the expression and theirs negations,
-- and it identifies Not(Not( E )) and E.
closure :: Expr -> Closure Expr
closure expr = closure' (rewriteExpr expr)

closure' :: Expr -> Closure Expr
closure' (Not e) = closure e

closure' expr@(Implies e1 e2) =
  [expr, Not expr] `union` closure e1 `union` closure e2

closure' expr@(Equiv e1 e2) =
  [expr, Not expr] `union` closure e1 `union` closure e2

closure' expr@(And e1 e2) =
  [expr, Not expr] `union` closure e1 `union` closure e2

closure' expr@(Or e1 e2) =
  [expr, Not expr] `union` closure e1 `union` closure e2

closure' expr@(Temporal(U e1 e2)) =
  [expr, Not expr] `union` closure e1 `union` closure e2

closure' expr@(Temporal(X e)) =
  [expr, Not expr] `union` closure e

closure' (Literal _) = [Literal True, Literal False]

closure' e = [e, Not e]


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
