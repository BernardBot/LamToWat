module Cps2Cps where

import Cps hiding (M)

import Control.Monad.Writer
import Control.Monad.Reader

import Data.Tuple
import Data.List

cps2cps :: Cps -> Cps
cps2cps =
  uncurry FIX .
  swap .
  flip runReader [] .
  runWriterT .
  c2c

type M =  WriterT [Fun] (Reader [String])

c2c :: Cps -> M Cps
c2c (FIX fs e) = do
  nv <- ask
  fs' <- mapM (\ (f,as,b) -> do
        b' <- local (++as) (c2c b)
        return (f,_nv:as,open b' as nv)) fs
  tell fs'
  c2c e
c2c (APP v vs) = do
  nv <- ask
  let vs' = rename vs
  let app = apply v vs'
  return (close app (v:vs) nv)
c2c (DONE v) = return (DONE v)
c2c (RECORD vs x e)  = local (++[x]) (c2c e >>= return . RECORD vs x)
c2c (SELECT i v x e) = local (++[x]) (c2c e >>= return . SELECT i v x)
c2c (ADD v1 v2 x e)  = local (++[x]) (c2c e >>= return . ADD v1 v2 x)

open :: Cps -> [String] -> [String] -> Cps
open e as nv = SELECT 1 (VAR _nv) _nv
  (foldr (\ (i,x) b -> if x `elem` as then b else SELECT i (VAR _nv) x b) e (zip [0..] nv))

apply :: Val -> [Val] -> Cps
apply (LABEL f) vs = APP (LABEL f) (VAR (_p f) : vs)
apply (VAR f) vs = SELECT 0 (VAR f) (_p f) (APP (VAR (_p f)) (VAR f:vs))

close :: Cps -> [Val] -> [String] -> Cps
close e vs nv = RECORD (map VAR nv) _nv
  (foldr (\ v b -> case v of LABEL f -> RECORD [v,VAR _nv] (_p f) b; _ -> b) e vs)

rename :: [Val] -> [Val]
rename vs = map (\ v -> case v of LABEL f -> VAR (_p f); _ -> v) vs

_p = ('_' :)
_nv = "_nv"
