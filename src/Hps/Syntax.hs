{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hps.Syntax where
  
import Val
import Interpreter
import Types

type Hps = Fix Exp

data Exp
  = APP Val [Val]
  | DONE Val
  | RECORD [Val] Var Exp
  | SELECT Int Val Var Exp
  | ADD Val Val Var Exp
  deriving (Eq,Show)

instance Interpretable Hps where
  interp (fs,e) = fix (map (fmap interp) fs,interp e)

instance Interpretable Exp where
  interp (APP v vs) = do
    Fun f <- interp v
    ds <- mapM interp vs
    f ds
  interp (ADD v1 v2 x e) = do
    Int i <- interp v1
    Int j <- interp v2
    letin x (Int $ i + j) (interp e)
  interp (RECORD vs x e) = do
    ds <- mapM interp vs
    letin x (Record ds) (interp e)
  interp (SELECT i v x e) = do
    Record ds <- interp v
    letin x (ds !! i) (interp e)
  interp (DONE v) = interp v

instance PPrintable Hps where
  pprint (fs,e) = concatMap pprint fs ++ pprint e

instance PPrintable (Fun Exp) where
  pprint (f,as,b) = "def " ++ f ++ args as ++ ":\n" ++ indent (pprint b)

instance PPrintable Exp where
  pprint (APP v vs)       = pprint v ++ args (map pprint vs)
  pprint (DONE v)         = "return " ++ pprint v
  pprint (ADD v1 v2 x e)  = assign x (pprint v1 ++ " + " ++ pprint v2)  ++ pprint e
  pprint (RECORD vs x e)  = assign x (recs (map pprint vs)            ) ++ pprint e
  pprint (SELECT n v x e) = assign x (pprint v ++ "[" ++ show n ++ "]") ++ pprint e
