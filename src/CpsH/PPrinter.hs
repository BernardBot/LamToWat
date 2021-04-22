module CpsH.PPrinter where

import Types

import CpsH.Syntax

pprint :: Expr -> String
pprint (fs,e) = concatMap pprintF fs ++ pprintExp e
  where pprintF (f,as,b) = "def " ++ f ++ args as ++ ":\n" ++ indent (pprintExp b)

pprintExp :: Exp -> String
pprintExp (APP v vs)       = pprintV v ++ args (map pprintV vs)
pprintExp (DONE v)         = "return " ++ pprintV v

pprintExp (ADD v1 v2 x e)  = assign x (pprintV v1 ++ " + " ++ pprintV v2) ++ pprintExp e
pprintExp (RECORD vs x e)  = assign x (recs (map pprintV vs)            ) ++ pprintExp e
pprintExp (SELECT n v x e) = assign x (pprintV v ++ "[" ++ show n ++ "]") ++ pprintExp e
