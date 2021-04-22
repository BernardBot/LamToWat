module CpsH.Interpreter where

import Types

import CpsH.Syntax

interp :: Expr -> IDom
interp (fs,e) = fix (map (fmap interpExp) fs,interpExp e)
  where interpExp (APP v vs) = do
         Fun f <- interpV v
         ds    <- mapM interpV vs
         f ds
        interpExp (ADD v1 v2 x e) = do
          Int i <- interpV v1
          Int j <- interpV v2
          letin x (Int $ i + j) (interpExp e)
        interpExp (RECORD vs x e) = do
          ds <- mapM interpV vs
          letin x (Record ds) (interpExp e)
        interpExp (SELECT i v x e) = do
          Record ds <- interpV v
          letin x (ds !! i) (interpExp e)
        interpExp (DONE v) = interpV v
