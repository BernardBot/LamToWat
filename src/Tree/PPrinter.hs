{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Tree.PPrinter where

import Control.Monad.State

import Types hiding (fix,Fix)

import Option
import Vec
import Union
import Commands

import Tree.Syntax

instance Show (Base n b p r q) where
  show (Add v1 v2) = "(Add " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (App v vs)  = "(App " ++ show v ++ " " ++ show vs ++ ")"

instance Show (Fix n b p r q) where
  show (Fix fxs)   = "(Fix " ++ show fxs ++ ")"

instance Show (Comp n b p r q) where
  show (SetK x v)  = "(SetK " ++ x ++ " " ++ show v ++ ")"
  show (GetK x)    = "(GetK " ++ x ++ ")"
  show Block       = "(Block)"
  show (Fresh x)   = "(Fresh " ++ x ++ ")"

---------------------------------------
--- show instance for command trees ---
---------------------------------------

type Cmd = Comp :+: Fix :+: Base

instance Show a => Show (Tree Cmd a) where
  show = fst . flip runState 0 . go
    where go :: Show a => Tree Cmd a -> State Int String
          go (Leaf v) = return $ show v
          go (Node (R (R op@(Add _ _))) Nil (Some k)) = do
            x <- fresh "x"
            k' <- go (k (VAR x))
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"
          go (Node (R (R op@(App _ _))) Nil None) = return $ show op
          go (Node (R (L op@(Fix _))) ks (Some k)) = do
            ks' <- mapM (go . ($())) ks
            k' <- go (k ())
            return $ show op ++ " [" ++
              foldr (\ k s -> k ++ "; " ++ s) "" ks' ++ "] " ++
              k'
          go (Node (L op@(GetK _)) Nil (Some k)) = do
            x <- fresh "x"
            k' <- go (k (VAR x))
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"
          go (Node (L op@(SetK _ _)) Nil (Some k)) = do
            x <- fresh "x"
            k' <- go (k ())
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"
          go (Node (L op@Block) (k0 ::: Nil) (Some k)) = do
            x <- fresh "x"
            k0' <- go (k0 ())
            k' <- go (k (VAR x))
            return $ show op ++ " [" ++
              k0' ++ "] (λ " ++ x ++ " → "
              ++ k' ++ ")"
          go (Node (L op@(Fresh y)) Nil (Some k)) = do
            x <- fresh y
            k' <- go (k x)
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"

          fresh :: String -> State Int String
          fresh s = do
            i <- get
            put (i+1)
            return $ "_" ++ s ++ show i

--------------
-- Examples --
--------------

e :: Tree Cmd Val
e = Node (R (L (Fix (("f", ["x"]) ::: Nil)))) ((\ () ->
      Node (R (R (Add (VAR "x") (INT 1)))) Nil (Some (\ n -> Leaf n))) ::: Nil) (Some (\ () ->
    Node (R (R (App (VAR "f") [INT 41]))) Nil None))
