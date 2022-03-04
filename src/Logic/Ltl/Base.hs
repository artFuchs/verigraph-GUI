{-# LANGUAGE OverloadedStrings #-}
module Logic.Ltl.Base
  ( Expr(..)
  , StateQuantified(..)
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



instance Pretty Expr where
  pretty = ppImplicative


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
