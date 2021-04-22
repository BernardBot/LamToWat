module Cps.Syntax where
  
import Types

data Expr
  = APP Val [Val]
  | DONE Val
  | RECORD [Val] String Expr
  | SELECT Int Val String Expr
  | ADD Val Val String Expr
  | FIX [Fun Expr] Expr
  deriving Show
