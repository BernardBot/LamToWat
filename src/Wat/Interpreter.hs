module Wat.Interpreter where

import Types

import Wat.Syntax

interp :: Expr -> IDom
interp (fs,e) = interpExp e
  where funs :: [IDom]
        funs = map (\ (f,as,b) -> function as (interpExp b)) fs

        interpExp :: Exp -> IDom
        interpExp (Malloc i x e) = do
          p <- malloc i
          letin x (Int p) (interpExp e)
        interpExp (Store i s t e) = do
          Int j <- interpV s
          d     <- interpV t
          store (i+j)xo d
          interpExp e
        interpExp (Load i v x e) = do
          Int j <- interpV v
          d <- load (i+j)
          letin x d (interpExp e)
        interpExp (Add v1 v2 x e) = do
          Int i <- interpV v1
          Int j <- interpV v2
          letin x (Int $ i + j) (interpExp e)
        interpExp (App v vs) = do
          Int fp <- interpV v
          Fun f  <- funs !! fp
          ds     <- mapM interpV vs
          f ds
        interpExp (Done v) = interpV v
