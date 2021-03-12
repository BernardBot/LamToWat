{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Tree2Tree where

import Tree

import Data.List
import Control.Monad

-- add this
hOther f (Leaf v) = Leaf v
hOther f (Node (R cmd) ks k) = Node cmd (fmap (\ k -> f k) ks) (fmap (\ k x -> f (k x)) k)

hBlock :: [(String,Val)] -> Tree (Block :+: Fresh :+: Fun :+: Base) Val -> Tree (Fresh :+: Fun :+: Base) Val
hBlock nv (BLOCK b k) = do
  r <- freshlabel "r"
  x <- freshvar "x"
  fun r [x] (hBlock nv (k x))
  v <- hBlock (("_nxt",r):nv) b
  app r [v]
hBlock nv (GETK x k) | Just v <- lookup x nv = hBlock nv (k v)
hBlock nv (SETK x v k) = hBlock ((x,v):nv) (k UNIT)
hBlock nv (Leaf v) = Leaf v
hBlock nv (Node (R cmd) ks k) =
  Node cmd
    (fmap (\ k -> hBlock nv k) ks)
    (fmap (\ k x -> hBlock nv (k x)) k)

-- how to get real fresh variables?
hFresh :: Int -> Tree (Fresh :+: sig) Val -> Tree sig Val
hFresh i (FRESHLABEL x k) =
  hFresh (i+1) (k (LABEL ("_" ++ x ++ show i)))
hFresh i (FRESHVAR x k) =
  hFresh (i+1) (k (VAR   ("_" ++ x ++ show i)))
hFresh i (Leaf v) = Leaf v
hFresh i (Node (R cmd) ks k) = do
  Node cmd
    (fmap (\ k -> hFresh (i+i*100) k) ks)
    (fmap (\ k x -> hFresh i (k x)) k)

_nv = VAR "_nv"
_p = ('_' :)

hClosure :: [Val] -> Tree (Fun :+: Base) Val -> Tree (Record :+: Fun :+: Base) Val
hClosure nv (Leaf v) = Leaf v
hClosure nv (FUN f as b k) = do
  fun f (_nv:as)
    (do select 1 _nv _nv
        zipWithM_ (\ i v -> if v `elem` as then return v else select i _nv v) [0..] nv
        hClosure (nv++as) b)
  hClosure nv (k f)
hClosure nv (APP v vs) = do
  record nv _nv
  vs' <- mapM (\ v -> case v of LABEL f -> record [v,_nv] (VAR (_p f)); _ -> return v) vs
  case v of
    VAR f -> do
      fp <- select 0 v (VAR (_p f))
      app fp (v:vs')
    LABEL f -> do
      fc <- record [v,_nv] (VAR (_p f))
      app v (fc:vs')
hClosure nv (ADD v1 v2 x k) = do
  add v1 v2 x
  hClosure (nv++[x]) (k x)

hRecord :: Tree (Record :+: sig) Val -> Tree (Malloc :+: sig) Val
hRecord (RECORD vs x k) = do
  malloc (length vs) x
  zipWithM_ (\ i v -> store i x v) [0..] vs
  hRecord (k x)
hRecord (SELECT i v x k) = do
  load i v x
  hRecord (k x)
hRecord (Leaf v) = Leaf v
hRecord (Node (R cmd) ks k) =
  Node (R cmd)
    (fmap (\ k -> hRecord k) ks)
    (fmap (\ k x -> hRecord (k x)) k)

type Tree' = Tree (Malloc :+: Base) Val

hFun :: Tree (Malloc :+: Fun :+: Base) Val -> ([(Val,[Val],Tree')], Tree')
hFun (Leaf v) = ([],Leaf v)
hFun (APP v vs) = ([],app v vs)
hFun (ADD v1 v2 x k) = case hFun (k x) of (fs,k') -> (fs, add v1 v2 x >> k')
hFun (MALLOC i x k) = case hFun (k x) of (fs,k') -> (fs, malloc i x >> k')
hFun (LOAD i v x k) = case hFun (k x) of (fs,k') -> (fs, load i v x >> k')
hFun (STORE i s t k) = case hFun (k UNIT) of (fs,k') -> (fs, store i s t >> k')
hFun (FUN f as b k) = case hFun b of (fs,b') -> case hFun (k f) of (fs',k') -> ((f,as,b'):fs'++fs,k')

