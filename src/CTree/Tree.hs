{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module CTree.Tree where

import Data.Void
import Data.List
import qualified Data.Tree as D

import Control.Monad
import Control.Monad.State

import Val
import Types (Var,Sig)
import qualified Types
import CTree.Vec
import CTree.Option
import CTree.Union
import CTree.Commands

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

--------------
-- Commands --
--------------

add :: Base :<: sig => Val -> Val -> Tree sig Val
add v1 v2 = liftT (inj (Add v1 v2)) Nil
app :: Base :<: sig => Val -> [Val] -> Tree sig a
app v vs = liftF (inj (App v vs)) Nil

fix :: Fix :<: sig => Vec n (Var,[Var],Tree sig Val) -> Tree sig ()
fix fs =
  let VPair (fxs, bs) =
        ifoldV (VPair (Nil, Nil))
               (\ (f, xs, b) (VPair (fxs, bs)) ->
                  VPair ((f, xs) ::: fxs, b ::: bs))
               fs
  in liftT (inj (Fix fxs)) (mapV const bs)
data VPair a b n = VPair { getVPair :: (Vec n a, Vec n b) }

setk :: Comp :<: sig => Var -> Val -> Tree sig ()
setk x v = liftT (inj (SetK x v)) Nil
getk :: Comp :<: sig => Var -> Tree sig Val
getk x = liftT (inj (GetK x)) Nil
block :: Comp :<: sig => Tree sig Val -> Tree sig Val
block b = liftT (inj Block) (const b ::: Nil)
fresh :: Comp :<: sig => String -> Tree sig String
fresh v = liftT (inj (Fresh v)) Nil

--------------
-- Printing --
--------------

type LamCmd = Comp :+: Fix :+: Base

indent :: String -> String
indent = unlines . map ("  "++) . lines

instance Show a => Show (Tree LamCmd a) where
  show = Types.runFresh . go
    where go :: Show a => Tree LamCmd a -> State Integer String
          go (Leaf v)                                 = return $ "Leaf (" ++ show v ++ ")"
          go (Node cmd@(R (R (App _ _))) Nil None)     = return $ show cmd

          go (Node cmd@(R (L (Fix _)))   ks (Some k)) = do
            ks' <- mapM (go . ($())) ks
            k' <- go (k ())
            return $ "Node (" ++ show cmd ++ ") (\n" ++
              indent (intercalate ":::" (toList ks' ++ ["Nil"])) ++ ") (\\ () ->\n" ++ k' ++ ")"
          go (Node cmd@(L Block) (k0 ::: Nil) (Some k)) = do
            x <- Types.fresh "x"
            k0' <- go (k0 ())
            k' <- go (k (VAR x))
            return $ "Node (" ++ show cmd ++ ") (\\ () -> \n" ++ indent k0' ++ ") (\\ " ++ x ++ " ->\n" ++ k' ++ ")"
          go (Node cmd@(L (Fresh y)) Nil (Some k)) = do
            x <- Types.fresh y
            k' <- go (k x)
            return $ "Node (" ++ show cmd ++ ") (\\ " ++ x ++ " ->\n" ++ k' ++ ")"

          go (Node cmd@(R (R (Add _ _))) Nil (Some k)) = showSimple cmd k VAR
          go (Node cmd@(L (GetK _))      Nil (Some k)) = showSimple cmd k VAR
          go (Node cmd@(L (SetK _ _))    Nil (Some k)) = showSimple cmd k (const ())

          showSimple cmd k f = do
            x <- Types.fresh "x"
            k' <- go (k (f x))
            return $ "Node (" ++ show cmd ++ ") (\\ " ++ x ++ " ->\n" ++ k' ++ ")"


instance Show a => Types.Treeable (Tree LamCmd a) where
  toTree = Types.runFresh . go
    where go :: Show a => Tree LamCmd a -> State Integer (D.Tree String)
          go (Leaf v) = return $ D.Node (show v) []
          go (Node cmd@(R (R (App _ _))) Nil None) =
            return $ D.Node (show cmd) []
          go (Node cmd@(R (L (Fix _)))   ks (Some k)) = do
            ks' <- mapM (go . ($())) ks
            k' <- go (k ())
            return $ D.Node (show cmd) [D.Node "ks" (toList ks'), k']
          go (Node cmd@(L Block) (k0 ::: Nil) (Some k)) = do
            x <- Types.fresh "x"
            k0' <- go (k0 ())
            k' <- go (k (VAR x))
            return $ D.Node (show cmd) [D.Node "k0" [k0'],k']
          go (Node cmd@(L (Fresh y)) Nil (Some k)) = do
            x <- Types.fresh y
            k' <- go (k x)
            return $ D.Node (x ++ " = " ++ show cmd) [k']

          go (Node cmd@(R (R (Add _ _))) Nil (Some k)) = showSimple cmd k VAR
          go (Node cmd@(L (GetK _))      Nil (Some k)) = showSimple cmd k VAR
          go (Node cmd@(L (SetK _ _))    Nil (Some k)) = showSimple cmd k (const ())

          showSimple cmd k f = do
            x <- Types.fresh "x"
            k' <- go (k (f x))
            return $ D.Node (x ++ " = " ++ show cmd)  [k']
