module Lam.PPrinter where

import Lam.Syntax

pprint :: Expr -> String
pprint (App e1 e2) = pprint e1 ++ " " ++
  case e2 of
    App {} -> "(" ++ pprint e2 ++ ")"
    _      -> pprint e2
pprint (Lam x e) = "(\\ " ++ x ++ go e
  where go e = case e of
          Lam x e -> " " ++ x ++ go e
          _       -> " -> " ++ pprint e ++ ")"
pprint (Var x) = x
pprint (Num i) = show i
pprint (Add e1 e2) = pprint e1 ++ " + " ++
    case e2 of
      App {} -> "(" ++ pprint e2 ++ ")"
      _      -> pprint e2
