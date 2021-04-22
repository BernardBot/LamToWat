module CpsH.Syntax where
  
import Types

type Expr = Fix Exp

data Exp
  = APP Val [Val]
  | DONE Val
  | RECORD [Val] String Exp
  | SELECT Int Val String Exp
  | ADD Val Val String Exp
  | FIX [Fun Exp] Exp
  deriving Show
