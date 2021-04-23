module Lam.Syntax where

import Types

data Expr
  = Lam Var Expr
  | App Expr Expr
  | Add Expr Expr
  | Var Var
  | Num Int
  deriving (Eq,Show)
