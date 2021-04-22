module Cps.Interpreter where

import Types

import Cps.Syntax

interp :: Expr -> IDom
interp (FIX fs e) =
  fix (map (fmap interp) fs,interp e)
interp (APP v vs) = do
  Fun f <- interpV v
  ds    <- mapM interpV vs
  f ds
interp (ADD v1 v2 x e) = do
  Int i <- interpV v1
  Int j <- interpV v2
  letin x (Int $ i + j) (interp e)
interp (RECORD vs x e) = do
  ds <- mapM interpV vs
  letin x (Record ds) (interp e)
interp (SELECT i v x e) = do
  Record ds <- interpV v
  letin x (ds !! i) (interp e)
interp (DONE v) = interpV v
