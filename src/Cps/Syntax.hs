module Cps.Syntax where
  
import Types

data Expr
  = APP Val [Val]
  | DONE Val
  | RECORD [Val] Var Expr
  | SELECT Int Val Var Expr
  | ADD Val Val Var Expr
  | FIX [Fun Expr] Expr
  deriving (Eq,Show)
