{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cont.Cps where

import Val
import Interpreter
import Types

data Cps
  = APP Val [Val]
  | DONE Val
  | RECORD [Val] Var Cps
  | SELECT Int Val Var Cps
  | ADD Val Val Var Cps
  | FIX [Fun Cps] Cps
  deriving (Eq,Show)

instance Interpretable Cps where
  interp (FIX fs e) =
    fix (map (fmap interp) fs,interp e)
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

instance Emitable Cps where
  emit (APP v vs)       = "return " ++ emit v ++ args (map emit vs)
  emit (DONE v)         = "return " ++ emit v
  emit (ADD v1 v2 x e)  = assign x (emit v1 ++ " + " ++ emit v2)  ++ emit e
  emit (RECORD vs x e)  = assign x (recs (map emit vs)            ) ++ emit e
  emit (SELECT n v x e) = assign x (emit v ++ "[" ++ show n ++ "]") ++ emit e
  emit (FIX fs e)       = concatMap emit fs ++ emit e

instance Emitable (Fun Cps) where
  emit (f,as,b) = "def " ++ f ++ args as ++ ":\n" ++ indent (emit b)
