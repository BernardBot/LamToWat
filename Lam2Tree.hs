{-# LANGUAGE TypeOperators #-}

module Lam2Tree where

import Lam hiding (INT)
import Tree hiding (Int)

lam2tree :: Lam -> Tree (Block :+: Fresh :+: Fun :+: Base) Val
lam2tree (Var x) = return (VAR x)
lam2tree (Int i) = return (INT i)
lam2tree (Lam x e) = do
  f <- freshlabel "f"
  k <- freshvar "k"
  fun f [VAR x,k]
    (do v <- lam2tree e
        app k [v])
lam2tree (App e1 e2) = block (do
  v1 <- lam2tree e1
  v2 <- lam2tree e2
  k <- getk "_nxt"
  app v1 [v2,k])
lam2tree (Add e1 e2) = do
  v1 <- lam2tree e1
  v2 <- lam2tree e2
  x <- freshvar "x"
  add v1 v2 x
