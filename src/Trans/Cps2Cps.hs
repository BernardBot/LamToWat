module Trans.Cps2Cps where

import Control.Monad.Writer
import Control.Monad.Reader

import Data.Tuple (swap)

import Types

import Cps.Syntax
import qualified CpsH.Syntax as H

type Cps = Cps.Syntax.Expr
type CpsH = H.Expr

type M =  WriterT [Fun H.Exp] (Reader [Var])

cps2cpsH :: Cps -> CpsH
cps2cpsH =
  swap .
  flip runReader [] .
  runWriterT .
  c2c

c2c :: Cps -> M H.Exp
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
c2c (DONE v) = return (H.DONE v)
c2c (RECORD vs x e)  = do
  e' <- local (++ [x]) (c2c e)
  return (H.RECORD vs x e')
c2c (SELECT i v x e) = do
  e' <- local (++ [x]) (c2c e)
  return (H.SELECT i v x e')
c2c (ADD v1 v2 x e)  = do
  e' <- local (++ [x]) (c2c e)
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
