module Lam2CpsM where

import Lam hiding (INT)
import CpsMonad
import Val

lam2cpsm :: Lam -> Cps Val
lam2cpsm (Var x) = done (VAR x)
lam2cpsm (Int i) = done (INT i)
lam2cpsm (Lam x e) = do
  f <- fresh "f"
  k <- fresh "k"
  fix [(f,[x,k],do
           v <- lam2cpsm e
           app (VAR k) [v])]
  done (LABEL f)
lam2cpsm (App e1 e2) = block (do
  v1 <- lam2cpsm e1
  v2 <- lam2cpsm e2
  k <- getk
  app v1 [v2,k])
lam2cpsm (Add e1 e2) = do
  v1 <- lam2cpsm e1
  v2 <- lam2cpsm e2
  add v1 v2

