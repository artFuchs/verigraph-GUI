{-# LANGUAGE OverloadedStrings #-}
module Logic.Ltl.Base
  ( Expr(..)
  , StateQuantified(..)
  ) where

import           Data.Text.Prettyprint.Doc

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
