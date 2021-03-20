{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module CpsS2CpsS where

import Control.Monad.Writer hiding (fix)
import Control.Monad.Reader hiding (fix)

import CpsUnion
import OpenUnion

import Data.Tuple
import Val

cpss2cpss =
  swap .
  flip runReader [] .
  runWriterT .
  c2c


type F = (String, [String], Fix (Record :+: Base))

type M =  WriterT [F] (Reader [String])


c2c :: Fix (Fun :+: Record :+: Base) -> M (Fix (Record :+: Base))
c2c (FIX fs e) = do
  nv <- ask
  fs' <- mapM (\ (f,as,b) -> do
        b' <- local (++ as) (c2c b)
        return (f,_nv:as,open b' as nv)) fs
  tell fs'
  c2c e
c2c (APP v vs) = do
  nv <- ask
  let vs' = rename vs
  let app = apply v vs'
  return (close app (v:vs) nv)
c2c (DONE v) = return (done v)
c2c (RECORD vs x e)  = do
  e' <- local (++ [x]) (c2c e)
  return (record vs x e')
c2c (SELECT i v x e) = do
  e' <- local (++ [x]) (c2c e)
  return (select i v x e')
c2c (ADD v1 v2 x e)  = do
  e' <- local (++ [x]) (c2c e)
  return (add v1 v2 x e')

open e as nv = select 1 (VAR _nv) _nv
  (foldr (\ (i,x) b -> if x `elem` as then b else select i (VAR _nv) x b) e (zip [0..] nv))

apply (LABEL f) vs = app (LABEL f) (VAR (_p f) : vs)
apply (VAR f) vs = select 0 (VAR f) (_p f) (app (VAR (_p f)) (VAR f : vs))


close e vs nv = record (map VAR nv) _nv
  (foldr (\ v b -> case v of LABEL f -> record [v,VAR _nv] (_p f) b; _ -> b) e vs)


rename vs = map (\ v -> case v of LABEL f -> VAR (_p f); _ -> v) vs

_p = ('_' :)
_nv = "_nv"
