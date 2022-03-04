module Logic.Ltl.Semantics (rewriteExpr) where

import Logic.Ltl.Base
import Logic.Ltl.Parser



-- rewrite expression in terms of X and U
rewriteExpr :: Expr -> Expr
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
  Not (Temporal (U (Not $ rewriteExpr e1) (Not $ rewriteExpr e2)))

rewriteExpr e = e
