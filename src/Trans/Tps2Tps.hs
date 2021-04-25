{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE GADTs #-}

module Trans.Tps2Tps where

import Data.Maybe

import Control.Monad

import Val
import Types (Var,Transformable,transform)
import qualified Types as T (Fix)

import Option
import Vec
import Union
import Commands

import Tps.Syntax
import Tps.Commands

type TpsClosA cmd v = Tps            (Fix :+: Base :+: cmd) v
type TpsClosB cmd v = Tps (Record :+: Fix :+: Base :+: cmd) v

instance Transformable (TpsClosA cmd v) (TpsClosB cmd v) where
  transform = transClos []

transClos :: [Var] -> Tps (Fix :+: Base :+: cmd) v -> Tps (Record :+: Fix :+: Base :+: cmd) v
transClos nv (Node (L (Fix fxs)) bs (Some (_,k))) = do
  let fxs' = mapV (\ (f,as) -> (f,"_nv":as)) fxs
      bs' = zipWithV (\ (f,as) b -> do
                       select_ 1 (VAR "_nv") "_nv"
                       zipWithM_ (\ i x ->
                                    if x `elem` as
                                    then return ()
                                    else select_ i (VAR "_nv") x) [0..] nv
                       transClos (nv ++ as) b) fxs bs
  fix' fxs' bs' (transClos nv k)

transClos nv (Node (R (L (App v vs))) Nil None) = do
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
transClos nv (Leaf v) = Leaf v
transClos nv (Node cmd ks k) =
  Node (R cmd)
    (fmap (transClos nv) ks)
    (fmap (\ (x,k) -> (x, transClos (nv ++ if null x then [] else [x]) k)) k)

instance Transformable (Tps (Record :+: cmd) v) (Tps (Malloc :+: cmd) v) where
  transform (Node (L (Record vs)) Nil (Some (x,k))) = do
    malloc_ (length vs) x
    zipWithM_ (\ i v -> store_ i (VAR x) v) [0..] vs
    transform k
  transform (Node (L (Select i v)) Nil (Some (x,k))) =
    load i v x (transform k)
  transform (Leaf v) = Leaf v
  transform (Node (R cmd) ks k) = Node (R cmd) (fmap transform ks) (fmap (fmap transform) k)

instance Transformable (Tps (Fix :+: cmd) Val) (T.Fix (Tps cmd Val)) where
  transform = transFix

transFix :: Tps (Fix :+: cmd) Val -> T.Fix (Tps cmd Val)
transFix (Leaf v) = ([],Leaf v)
transFix (Node (R cmd) ks k) = case fmap (fmap transFix) k of
  Some (x,(fs,k')) -> (fs++fs',Node cmd ks' (Some (x,k')))
  None -> (fs',Node cmd ks' None)
  where ks' = fmap (snd . transFix) ks
        fs' = concat (toList (fmap (fst . transFix) ks))
transFix (Node (L (Fix fxs)) bs (Some ("", k))) = let (fs,k') = transFix k in (fs'++fs,k')
  where fs' = concatMap (\ ((f,as),b) -> let (fs,b') = transFix b in (f,as,b') : fs) (zip (toList fxs) (toList bs))
