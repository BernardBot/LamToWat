{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE GADTs #-}

module Trans.Tps2Tps where

import Data.Maybe

import Control.Monad

import Types (Val(INT,VAR,LABEL),Var,Transformable,transform)
import qualified Types as T (Fix)

import Option
import Vec
import Union
import Commands

import Tps.Syntax
import Tps.Commands

type TpsClosA cmd = Tps            (Fix :+: Base :+: cmd) Val
type TpsClosB cmd = Tps (Record :+: Fix :+: Base :+: cmd) Val

instance Transformable (TpsClosA cmd) (TpsClosB cmd) where
  transform = hClos []

hClos :: [Var] -> Tps (Fix :+: Base :+: cmd) Val -> Tps (Record :+: Fix :+: Base :+: cmd) Val
hClos nv (Node (L (Fix fxs)) bs (Some (_,k))) = do
  let fxs' = mapV (\ (f,as) -> (f,"_nv":as)) fxs
      bs' = zipWithV (\ (f,as) b -> do
                       select_ 1 (VAR "_nv") "_nv"
                       zipWithM_ (\ i x ->
                                    if x `elem` as
                                    then return ()
                                    else select_ i (VAR "_nv") x) [0..] nv
                       hClos (nv ++ as) b) fxs bs
  fix' fxs' bs' (hClos nv k)

hClos nv (Node (R (L (App v vs))) Nil None) = do
  record_ (map VAR nv) "_nv"
  vs' <- mapM (\ v -> case v of
                  LABEL f -> do
                    let _f = "_" ++ f
                    record_ [v,VAR "_nv"] _f
                    return (VAR _f)
                  _       -> return v) vs
  case v of
    LABEL f -> do
      let fc = "_" ++ f
      record_ [v,VAR "_nv"] fc
      app v (VAR fc:vs')
    VAR f -> do
      let fp = "_" ++ f
      select_ 0 v fp
      app (VAR fp) (v:vs')
hClos nv (Leaf v) = Leaf v
hClos nv (Node cmd ks k) =
  Node (R cmd)
    (fmap (hClos nv) ks)
    (fmap (\ (x,k) -> (x, hClos (nv ++ if null x then [] else [x]) k)) k)

instance Transformable (Tps (Record :+: cmd) Val) (Tps (Malloc :+: cmd) Val) where
  transform (Node (L (Record vs)) Nil (Some (x,k))) = do
    malloc_ (length vs) x
    zipWithM_ (\ i v -> store_ i (VAR x) v) [0..] vs
    transform k
  transform (Node (L (Select i v)) Nil (Some (x,k))) =
    load i v x (transform k)
  transform (Leaf v) = Leaf v
  transform (Node (R cmd) ks k) = Node (R cmd) (fmap transform ks) (fmap (fmap transform) k)

instance Transformable (Tps (Fix :+: cmd) Val) (T.Fix (Tps cmd Val)) where
  transform = hFix

hFix :: Tps (Fix :+: cmd) Val -> T.Fix (Tps cmd Val)
hFix (Leaf v) = ([],Leaf v)
hFix (Node (R cmd) ks k) = case fmap (fmap hFix) k of
  Some (x,(fs,k')) -> (fs++fs',Node cmd ks' (Some (x,k')))
  None             -> (    fs',Node cmd ks' None)
  where ks' = fmap (snd . hFix) ks
        fs' = concat (toList (fmap (fst . hFix) ks))
hFix (Node (L (Fix fxs)) bs (Some ("", k))) = let (fs,k') = hFix k in (fs'++fs,k')
  where fs' = concatMap (\ ((f,as),b) -> let (fs,b') = hFix b in (f,as,b') : fs) (zip (toList fxs) (toList bs))
