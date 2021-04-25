module Trans.Cps2Cps where

import Control.Monad.Writer
import Control.Monad.Reader

import Data.Tuple

import Val
import Types

import Cps.Syntax

cps2cps :: Cps -> Cps
cps2cps =
  uncurry FIX .
  swap .
  flip runReader [] .
  runWriterT .
  cps2cps'

type TransM = WriterT [Fun Cps] (Reader [Var])

cps2cps' :: Cps -> TransM Cps
cps2cps' (FIX fs e) = do
  nv <- ask
  fs' <- mapM (\ (f,as,b) -> do
        b' <- local (++ as) (cps2cps' b)
        return (f,_nv:as,open b' as nv)) fs
  tell fs'
  cps2cps' e
cps2cps' (APP v vs) = do
  nv <- ask
  let vs' = rename vs
  let app = apply v vs'
  return (close app (v:vs) nv)
cps2cps' (DONE v) = return (DONE v)
cps2cps' (RECORD vs x e)  = do
  e' <- local (++ [x]) (cps2cps' e)
  return (RECORD vs x e')
cps2cps' (SELECT i v x e) = do
  e' <- local (++ [x]) (cps2cps' e)
  return (SELECT i v x e')
cps2cps' (ADD v1 v2 x e)  = do
  e' <- local (++ [x]) (cps2cps' e)
  return (ADD v1 v2 x e')

open :: Cps -> [Var] -> [Var] -> Cps
open e as nv = SELECT 1 (VAR _nv) _nv
  (foldr (\ (i,x) b -> if x `elem` as then b else SELECT i (VAR _nv) x b) e (zip [0..] nv))

apply :: Val -> [Val] -> Cps
apply (LABEL f) vs = APP (LABEL f) (VAR (_p f) : vs)
apply (VAR f) vs = SELECT 0 (VAR f) (_p f) (APP (VAR (_p f)) (VAR f : vs))

close :: Cps -> [Val] -> [Var] -> Cps
close e vs nv = RECORD (map VAR nv) _nv
  (foldr (\ v b -> case v of LABEL f -> RECORD [v,VAR _nv] (_p f) b; _ -> b) e vs)

rename :: [Val] -> [Val]
rename vs = map (\ v -> case v of LABEL f -> VAR (_p f); _ -> v) vs

_p = ('_' :)
_nv = "_nv"
