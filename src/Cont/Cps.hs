{-# LANGUAGE DeriveDataTypeable #-}

module Cont.Cps where

import Data.Data

import Val
import Interpreter
import Types

data Cps
  = App Val [Val]
  | Val Val
  | Record [Val] Var Cps
  | Select Int Val Var Cps
  | Add Val Val Var Cps
  | Fix [Fun Cps] Cps
  deriving (Eq,Show,Data)

instance Interpretable Cps where
  interp (Fix fs e) =
    fix (map (fmap interp) fs,interp e)
  interp (App v vs) = do
    Fun f <- interp v
    ds <- mapM interp vs
    f ds
  interp (Add v1 v2 x e) = do
    Int i <- interp v1
    Int j <- interp v2
    letin x (Int $ i + j) (interp e)
  interp (Cont.Cps.Record vs x e) = do
    ds <- mapM interp vs
    letin x (Interpreter.Record ds) (interp e)
  interp (Select i v x e) = do
    Interpreter.Record ds <- interp v
    letin x (ds !! i) (interp e)
  interp (Val v) = interp v
