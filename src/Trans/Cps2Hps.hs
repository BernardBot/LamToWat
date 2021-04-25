{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trans.Cps2Hps where

import Control.Monad.Writer
import Control.Monad.Reader

import Data.Tuple

import Val
import Types

import Cps.Syntax
import Hps.Syntax

import qualified Cps.Syntax as C
import qualified Hps.Syntax as H

type TransM = WriterT [Fun H.Exp] (Reader [Var])

instance Transformable Cps Hps where
  transform =
    swap .
    flip runReader [] .
    runWriterT .
    trans

trans :: Cps -> TransM H.Exp
trans (C.FIX fs e) = do
  nv <- ask
  fs' <- mapM (\ (f,as,b) -> do
        b' <- local (++ as) (trans b)
        return (f,_nv:as,open b' as nv)) fs
  tell fs'
  trans e
trans (C.APP v vs) = do
  nv <- ask
  let vs' = rename vs
  let app = apply v vs'
  return (close app (v:vs) nv)
trans (C.DONE v) = return (H.DONE v)
trans (C.RECORD vs x e)  = do
  e' <- local (++ [x]) (trans e)
  return (H.RECORD vs x e')
trans (C.SELECT i v x e) = do
  e' <- local (++ [x]) (trans e)
  return (H.SELECT i v x e')
trans (C.ADD v1 v2 x e)  = do
  e' <- local (++ [x]) (trans e)
  return (H.ADD v1 v2 x e')

open :: H.Exp -> [Var] -> [Var] -> H.Exp
open e as nv = H.SELECT 1 (VAR _nv) _nv
  (foldr (\ (i,x) b -> if x `elem` as then b else H.SELECT i (VAR _nv) x b) e (zip [0..] nv))

apply :: Val -> [Val] -> H.Exp
apply (LABEL f) vs = H.APP (LABEL f) (VAR (_p f) : vs)
apply (VAR f) vs = H.SELECT 0 (VAR f) (_p f) (H.APP (VAR (_p f)) (VAR f : vs))

close :: H.Exp -> [Val] -> [Var] -> H.Exp
close e vs nv = H.RECORD (map VAR nv) _nv
  (foldr (\ v b -> case v of LABEL f -> H.RECORD [v,VAR _nv] (_p f) b; _ -> b) e vs)

rename :: [Val] -> [Val]
rename vs = map (\ v -> case v of LABEL f -> VAR (_p f); _ -> v) vs

_p = ('_' :)
_nv = "_nv"
