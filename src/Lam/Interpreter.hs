module Lam.Interpreter where

import Lam.Syntax
import Types

interp :: Expr -> IDom
interp (Var x) = look x
interp (Num i) = int i
interp (Lam x e) = closure [x] (interp e)
interp (App e1 e2) = do
  Fun f <- interp e1
  a     <- interp e2
  f [a]
interp (Add e1 e2) = do
  Int i <- interp e1
  Int j <- interp e2
  int $ i + j
