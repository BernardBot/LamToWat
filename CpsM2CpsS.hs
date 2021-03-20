{-# LANGUAGE TypeOperators #-}

module CpsM2CpsS where

import CpsMonad hiding (done,app,add,record,select,fix,fresh,getk,setk,block)
import CpsUnion hiding (DONE,APP,ADD,RECORD,SELECT,FIX)

import Control.Monad.Reader hiding (fix)
import Control.Monad.State hiding (fix)

import Val
import OpenUnion

bar :: Cps Val -> Fix (Fun :+: Record :+: Base)
bar = fst . flip runReader (INT 0) . flip runStateT 0 . foo

fresh :: (MonadState a m, Num a, Show a) => [Char] -> m [Char]
fresh s = do
  i <- get
  put (i+1)
  return $ "_" ++ s ++ show i

foo :: Cps Val -> StateT Int (Reader Val) (Fix (Fun :+: Record :+: Base))
foo (DONE v) = return (done v)
foo (APP v vs) = return (app v vs)
foo (ADD v1 v2 k) = do
  x <- fresh "x"
  k' <- foo (k (VAR x))
  return (add v1 v2 x k')
foo (RECORD vs k) = do
  x <- fresh "x"
  k' <- foo (k (VAR x))
  return (record vs x k')
foo (SELECT i v k) = do
  x <- fresh "x"
  k' <- foo (k (VAR x))
  return (select i v x k')
foo (FIX fs k) = do
  fs' <- mapM (\ (f,as,b) -> do b' <- foo b; return (f,as,b')) fs
  k' <- foo k
  return (fix fs' k')
foo (FRESH x k) = do
  f <- fresh "x"
  foo (k f)
foo (GETK k) = do
  nxt <- ask
  foo (k nxt)
foo (SETK v k) = do
  local (const v) (foo k)
foo (BLOCK b k) = do
  r <- fresh "r"
  x <- fresh "x"
  b' <- local (const (LABEL r))
        (foo (do v <- b
                 APP (LABEL r) [v]))
  k' <- foo (k (VAR x))
  return (fix [(r,[x],k')] b')
