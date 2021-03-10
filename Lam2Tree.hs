{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Lam2Tree where

import Lam hiding (INT)
import Tree

lam2tree :: Lam -> Tree (Block :+: Fun :+: Base) Val
lam2tree (Var x) = return (VAR x)
lam2tree (Int i) = return (INT i)
lam2tree (Lam x e) = do
  fun 2 (\ [f,x',k] -> do
            set x x'
            v <- lam2tree e
            app k [v])
lam2tree (App e1 e2) = block (do
  v1 <- lam2tree e1
  v2 <- lam2tree e2
  k <- getk "_nxt"
  app v1 [v2,k])
lam2tree (Add e1 e2) = do
  v1 <- lam2tree e1
  v2 <- lam2tree e2
  add v1 v2
