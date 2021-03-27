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

hClos' :: [String] -> Tps (Fix :+: Base) Val -> Tps (Record :+: Fix :+: Base) Val
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

hClos' nv (Node (R (App v vs)) Nil None) = do
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

type Tps' = Tps (Malloc :+: Base) Val

hFun :: Tps (Malloc :+: Fix :+: Base) Val -> ([(String,[String],Tps')],Tps')
hFun (Leaf v)                                         = ([],Leaf v)
hFun (Node (L (Malloc i))      Nil (Some (Some x,k))) = let (fs,b) = hFun k in (fs,malloc i x b)
hFun (Node (L (Load i v))      Nil (Some (Some x,k))) = let (fs,b) = hFun k in (fs,load i v x b)
hFun (Node (L (Store i s t))   Nil (Some (_,k)))      = let (fs,b) = hFun k in (fs,store i s t b)
hFun (Node (R (R (Add v1 v2))) Nil (Some (Some x,k))) = let (fs,b) = hFun k in (fs,add v1 v2 x b)
hFun (Node (R (R (App v vs)))  Nil None)              = ([],app v vs)
hFun (Node (R (L (Fix fxs)))   bs  (Some (_,k)))      = let (fs,b) = hFun k in (fs'++fs,b)
  where fs' = concatMap (\ ((f,as),b) -> let (fs,b') = hFun b in (f,as,b') : fs) (zip (toList fxs) (toList bs))
