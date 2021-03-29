{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Tps2Tps where

import Val
import Option
import Vec
import Tps
import Data.Maybe

import Control.Monad

hClos = hClos' []

hClos' :: [String] -> Tps (Fix :+: Base :+: cmd) Val -> Tps (Record :+: Fix :+: Base :+: cmd) Val
hClos' nv (Node (L (Fix fxs)) bs (Some (_,k))) = do
  let fxs' = mapV (\ (f,as) -> (f,"_nv":as)) fxs
      bs' = zipWithV (\ (f,as) b -> do
                       select_ 1 (VAR "_nv") "_nv"
                       zipWithM_ (\ i x ->
                                    if x `elem` as
                                    then return ()
                                    else select_ i (VAR "_nv") x) [0..] nv
                       hClos' (nv ++ as) b) fxs bs
  fix fxs' bs' (hClos' nv k)

hClos' nv (Node (R (L (App v vs))) Nil None) = do
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
hClos' nv (Leaf v) = Leaf v
hClos' nv (Node cmd ks k) =
  Node (R cmd)
    (mapV (\ k -> hClos' nv k) ks)
    (fmap (\ (o,k) -> (o,case o of
                          Some x -> hClos' (nv ++ [x]) k
                          None   -> hClos' nv          k)) k)

hRecord :: Tps (Record :+: cmd) Val -> Tps (Malloc :+: cmd) Val
hRecord (Node (L (Record vs)) Nil (Some (Some x,k))) = do
  malloc_ (length vs) x
  zipWithM_ (\ i v -> store_ i (VAR x) v) [0..] vs
  hRecord k
hRecord (Node (L (Select i v)) Nil (Some (Some x,k))) =
  load i v x (hRecord k)
hRecord (Leaf v) = Leaf v
hRecord (Node (R cmd) ks k) =
  Node (R cmd)
    (mapV (\ k -> hRecord k) ks)
    (fmap (\ (o,k) -> (o,hRecord k)) k)

hFun :: Tps (Fix :+: cmd) Val -> ([(String,[String],Tps cmd Val)],Tps cmd Val)
hFun (Leaf v) = ([],Leaf v)
hFun (Node (R cmd) Nil k) = case fmap (fmap hFun) k of
  Some (x,(fs,k')) -> (fs,Node cmd Nil (Some (x,k')))
  None             -> ([],Node cmd Nil None)
hFun (Node (L (Fix fxs)) bs (Some (None, k))) = let (fs,k') = hFun k in (fs'++fs,k')
  where fs' = concatMap (\ ((f,as),b) -> let (fs,b') = hFun b in (f,as,b') : fs) (zip (toList fxs) (toList bs))
