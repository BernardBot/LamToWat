module Lam.Syntax where

import Val
import Interpreter
import Types

data Lam
  = Lam Var Lam
  | App Lam Lam
  | Add Lam Lam
  | Val Val
  deriving (Eq,Show)

instance Interpretable Lam where
  interp (Val v) = interp v
  interp (App e1 e2) = do
    Fun f <- interp e1
    a <- interp e2
    f [a]
  interp (Add e1 e2) = do
    Int i <- interp e1
    Int j <- interp e2
    int $ i + j

instance PPrintable Lam where
  pprint (Val v) = pprint v
  pprint (App e1 e2) = pprint e1 ++ " " ++
    case e2 of
      App {} -> "(" ++ pprint e2 ++ ")"
      _      -> pprint e2
  pprint (Lam x e) = "(\\ " ++ x ++ go e
    where go e = case e of
            Lam x e -> " " ++ x ++ go e
            _       -> " -> " ++ pprint e ++ ")"
  pprint (Add e1 e2) = pprint e1 ++ " + " ++
    case e2 of
      App {} -> "(" ++ pprint e2 ++ ")"
      _      -> pprint e2
