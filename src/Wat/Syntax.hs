module Wat.Syntax where

import Types

type Expr = Fix Exp

data Exp
  = Malloc Int String Exp
  | Store Int Val Val Exp
  | Load Int Val String Exp
  | Add Val Val String Exp
  | App Val [Val]
  | Done Val
  deriving Show
