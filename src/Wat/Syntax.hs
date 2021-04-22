module Wat.Syntax where

import Types

type Expr = Fix Exp

type Offset = Int
data Exp
  = Malloc Int Var Exp
  | Store Offset Val Val Exp
  | Load Offset Val Var Exp
  | Add Val Val Var Exp
  | App Val [Val]
  | Done Val
  deriving Show
