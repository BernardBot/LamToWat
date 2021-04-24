{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Trans.Lam2Tree where

import Types (Val(INT,VAR,LABEL),Transformable,transform)
import Union
import Vec
import Commands (Comp,Fix,Base)

import Tree.Syntax
import Tree.Commands

import Lam.Syntax

type LamTree = Tree (Comp :+: Fix :+: Base) Val

instance Transformable Lam LamTree where
  transform (Val v) = return v
  transform (Lam x e) = do
    f <- fresh "f"
    k <- fresh "k"
    fix ((f,[x,k],do
             v <- transform e
             app (VAR k) [v])
         ::: Nil)
    return (LABEL f)
  transform (App e1 e2) = block (do
    v1 <- transform e1
    v2 <- transform e2
    k <- getk "_nxt"
    app v1 [v2,k])
  transform (Add e1 e2) = do
    v1 <- transform e1
    v2 <- transform e2
    add v1 v2

