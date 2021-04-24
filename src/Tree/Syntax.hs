{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Tree.Syntax where

import Data.Void

import Control.Monad
import Control.Monad.State

import Types hiding (Fix)
import Vec
import Option
import Union
import Commands

data Tree (sig :: Sig) a where
  Leaf :: a -> Tree sig a
  Node :: sig n b p r q ->
          Vec n (p -> Tree sig r) ->
          Option b (q -> Tree sig a) ->
          Tree sig a

instance Monad (Tree sig) where
  Leaf x       >>= g = g x
  Node op cs k >>= g = Node op cs (fmap (\ k x -> k x >>= g) k)

instance Functor (Tree sig) where
  fmap = liftM

instance Applicative (Tree sig) where
  pure = Leaf
  (<*>) = ap
  
liftT :: sig n 'True p r a -> Vec n (p -> Tree sig r) -> Tree sig a
liftT op ps = Node op ps (Some Leaf)

liftF :: sig n 'False p r q -> Vec n (p -> Tree sig r) -> Tree sig a
liftF op ps = Node op ps None

-- TODO: make printing modular

type LamCmd = Comp :+: Fix :+: Base

instance Show a => Show (Tree LamCmd a) where
  show = runFresh . go
    where go :: Show a => Tree LamCmd a -> State Integer String
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
