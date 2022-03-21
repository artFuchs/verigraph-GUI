{-# LANGUAGE OverloadedStrings #-}
module Logic.Ltl.Base
  ( Expr(..)
  , StateQuantified(..)
  , rewriteExpr
  ) where

import Data.Text.Prettyprint.Doc

data Expr
  = Literal Bool
  | Atom String
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Implies Expr Expr
  | Equiv Expr Expr
  | Temporal (StateQuantified Expr)
  deriving (Eq, Show, Read)


data StateQuantified e
  = X e
  | F e
  | G e
  | U e e
  | W e e
  | R e e
  deriving (Eq, Show, Read)


instance Ord Expr where
  (<=) = leq

instance Pretty Expr where
  pretty = ppImplicative

exprLength :: Expr -> Int
exprLength (Atom _) = 1
exprLength (Literal _) = 1
exprLength (Not e) = 1 + exprLength e
exprLength (Temporal (X e)) = 1 + exprLength e
exprLength (Temporal (F e)) = 1 + exprLength e
exprLength (Temporal (G e)) = 1 + exprLength e
exprLength (Temporal (e1 `U` e2)) = 1 + exprLength e1 + exprLength e2
exprLength (Temporal (e1 `W` e2)) = 1 + exprLength e1 + exprLength e2
exprLength (Temporal (e1 `R` e2)) = 1 + exprLength e1 + exprLength e2
exprLength (Implies e1 e2) = 1 + exprLength e1 + exprLength e2
exprLength (Equiv e1 e2) = 1 + exprLength e1 + exprLength e2
exprLength (And e1 e2) = 1 + exprLength e1 + exprLength e2
exprLength (Or e1 e2) = 1 + exprLength e1 + exprLength e2


leq :: Expr -> Expr -> Bool
(Literal False) `leq` (Literal True) = True
(Literal True) `leq` (Literal False) = False
(Literal True) `leq` _ = True
(Literal _) `leq` (Atom _) = True
(Atom _) `leq` (Literal _) = False
(Atom str1) `leq` (Atom str2) = str1 <= str2
(Atom _) `leq` _ = True
expr1 `leq` expr2 =
  let
    len1 = exprLength expr1
    len2 = exprLength expr2
  in
    case (len1 < len2, len1 > len2) of
      (True,False) -> True
      (False, True) -> False
      _ ->
        case (expr1, expr2) of
          (Not e1, Not e2) -> e1 `leq` e2
          (And a1 a2, And b1 b2) -> a1 `leq` b1 || a2 `leq` b2
          (Or a1 a2, Or b1 b2) -> a1 `leq` b1 || a2 `leq` b2
          (Implies a1 a2, Implies b1 b2) -> a1 `leq` b1 || a2 `leq` b2
          (Equiv a1 a2, Equiv b1 b2) -> a1 `leq` b1 || a2 `leq` b2
          (Temporal (X e1), Temporal (X e2)) -> e1 `leq` e2
          (Temporal (F e1), Temporal (F e2)) -> e1 `leq` e2
          (Temporal (G e1), Temporal (G e2)) -> e1 `leq` e2
          (Temporal (U a1 a2), Temporal (U b1 b2)) -> a1 `leq` b1 || a2 `leq` b2
          (Temporal (W a1 a2), Temporal (W b1 b2)) -> a1 `leq` b1 || a2 `leq` b2
          (Temporal (R a1 a2), Temporal (R b1 b2)) -> a1 `leq` b1 || a2 `leq` b2
          (Not _, _) -> True
          (_, Not _) -> False
          (And _ _, _) -> True
          (_, And _ _) -> False
          (Or _ _, _) -> True
          (_, Or _ _) -> False
          (Implies _ _, _) -> True
          (_, Implies _ _) -> False
          (Equiv _ _, _) -> True
          (_, Equiv _ _) -> False
          (Temporal (X _), _) -> True
          (_, Temporal (X _)) -> False
          (Temporal (F _), _) -> True
          (_, Temporal (F _)) -> False
          (Temporal (G _), _) -> True
          (_, Temporal (G _)) -> False
          (Temporal (U _ _), _) -> True
          (_, Temporal (U _ _)) -> False
          (Temporal (W _ _), _) -> True
          (_, Temporal (W _ _)) -> False
          (Temporal (R _ _), _) -> True
          (_, Temporal (R _ _)) -> False
          _ -> False


ppImplicative :: Expr -> Doc ann
ppImplicative (Implies e1 e2) = group . align $ fillSep [ppBoolean e1, "->" <+> ppImplication e2]
  where
    ppImplication (Implies e1 e2) = fillSep [ppBoolean e1, "->" <+> ppImplication e2]
    ppImplication e               = ppBoolean e
ppImplicative (Equiv e1 e2) = ppInfix (ppBoolean e1) "<->" (ppBoolean e2)
ppImplicative e = ppBoolean e

ppBoolean :: Expr -> Doc ann
ppBoolean e@(And _ _) = group . align $ ppAnd e
  where
    ppAnd (And e1 e2) = fillSep [ppUnary e1, "&&" <+> ppAnd e2]
    ppAnd e           = ppUnary e
ppBoolean e@(Or _ _) = group . align $ ppOr e
  where
    ppOr (Or e1 e2) = fillSep [ppUnary e1, "||" <+> ppOr e2]
    ppOr e          = ppUnary e
ppBoolean e = ppUnary e

ppUnary :: Expr -> Doc ann
ppUnary (Not e)      = "~" <> ppUnary e
ppUnary (Temporal e) = ppTemporal e
ppUnary e            = ppAtomic e

ppAtomic :: Expr -> Doc ann
ppAtomic (Literal True)  = "true"
ppAtomic (Literal False) = "false"
ppAtomic (Atom prop)     = pretty prop
ppAtomic e               = parens (pretty e)

ppTemporal :: StateQuantified Expr -> Doc ann
ppTemporal (X e) = "X" <+> ppUnary e
ppTemporal (F e) = "F" <+> ppUnary e
ppTemporal (G e) = "G" <+> ppUnary e
ppTemporal (U e1 e2) =
  parens $ sep [pretty e1, "U" <+> pretty e2]
ppTemporal (W e1 e2) =
  parens $ sep [pretty e1, "W" <+> pretty e2]
ppTemporal (R e1 e2) =
  parens $ sep [pretty e1, "R" <+> pretty e2]

ppInfix :: Doc ann -> Doc ann -> Doc ann -> Doc ann
ppInfix e1 op e2 = align $ sep [e1, op <+> e2]



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
