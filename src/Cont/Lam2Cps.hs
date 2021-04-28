module Cont.Lam2Cps where

import Control.Monad.Cont
import Control.Monad.State

import Val
import Types (fresh)

import Lam
import Cont.Cps


import qualified Lam as L
import qualified Cont.Cps as C

lam2cps :: Lam -> Cps
lam2cps =
    fst .
    flip runState 0 .
    flip l2c (return . C.Val)

l2c :: Lam -> (Val -> State Int Cps) -> State Int Cps
l2c (L.Val v) c = c v
l2c (L.Lam x e) c = do
  f <- fresh "f"
  k <- fresh "k"
  c' <- c (LABEL f)
  cf <- l2c e $ \ z -> return $ C.App (VAR k) [z]
  return $ C.Fix [(f,[x,k],cf)] c'
l2c (L.App e1 e2) c = do
  r <- fresh "r"
  x <- fresh "x"
  c' <- c (VAR x)
  cf <- l2c e1 $ \ v1 -> l2c e2 $ \ v2 -> return $ C.App v1 [v2, LABEL r]
  return $ C.Fix [(r,[x],c')] cf
l2c (L.Add e1 e2) c = do
  x <- fresh "x"
  c' <- c (VAR x)
  l2c e1 $ \ v1 -> l2c e2 $ \ v2 -> return $ C.Add v1 v2 x c'
