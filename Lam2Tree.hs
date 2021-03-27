module Lam2Tree where

import Val
import Lam hiding (INT)
import Tree hiding (App,Add)
import Vec

lam2tree :: Lam -> Tree Cmd Val
lam2tree (Var x) = return (VAR x)
lam2tree (Num i) = return (INT i)
lam2tree (Lam x e) = do
  f <- fresh "f"
  k <- fresh "k"
  fix ((f,[x,k],do
           v <- lam2tree e
           app (VAR k) [v])
       ::: Nil)
  return (LABEL f)
lam2tree (App e1 e2) = block (do
  v1 <- lam2tree e1
  v2 <- lam2tree e2
  k <- getk "_nxt"
  app v1 [v2,k])
lam2tree (Add e1 e2) = do
  v1 <- lam2tree e1
  v2 <- lam2tree e2
  add v1 v2
