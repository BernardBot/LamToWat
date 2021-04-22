module Cps.PPrinter where

import Types

import Cps.Syntax

pprint :: Expr -> String
pprint (APP v vs)       = pprintV v ++ args (map pprintV vs)
pprint (DONE v)         = "return " ++ pprintV v

pprint (ADD v1 v2 x e)  = assign x (pprintV v1 ++ " + " ++ pprintV v2) ++ pprint e
pprint (RECORD vs x e)  = assign x (recs (map pprintV vs)            ) ++ pprint e
pprint (SELECT n v x e) = assign x (pprintV v ++ "[" ++ show n ++ "]") ++ pprint e

pprint (FIX fs e)       = concatMap pprintF fs ++ pprint e
  where pprintF (f,as,b) = "def " ++ f ++ args as ++ ":\n" ++ indent (pprint b)

