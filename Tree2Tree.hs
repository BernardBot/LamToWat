{-# LANGUAGE GADTs #-}

module Tree2Tree where

import Tree

import Data.List
import Control.Monad

hBlock nv (BLOCK b k) = do
  r <- fun 1 (\ [r,x] -> hBlock nv (k x))
  v <- hBlock (("_nxt",r):nv) (b [])
  app r [v]
hBlock nv (GETK x k) | Just v <- lookup x nv = hBlock nv (k v)
hBlock nv (SETK x v k) = hBlock ((x,v):nv) (k v)
hBlock nv (Leaf v) = Leaf v
hBlock nv (Node (R cmd) ks k) =
  Node cmd
    (fmap (\ k x -> hBlock nv (k x)) ks)
    (fmap (\ k x -> hBlock nv (k x)) k)

hClosure nv (FUN i b k) = do
  f <- fun (i+1) (\ (f:c:as) -> do
                _c <- select 1 c
                zipWithM
                  (\ i x -> do _x <- select i _c; set x _x) [0..]
                  (concatMap val2strl (nub nv \\ as))
                hClosure (nub (as++nv)) (b (f:as)))
  hClosure nv (k f)
hClosure nv (APP v vs) = do
  c <- record nv
  vs' <- mapM (\ v -> case v of LABEL f -> record [v,c]; _ -> return v) vs
  case v of
    VAR f -> do
      fp <- select 0 v
      app fp (v:vs')
    LABEL f -> do
      fc <- record [v,c]
      app v (fc:vs')
hClosure nv (Leaf v) = Leaf v
hClosure nv (Node cmd ks k) =
  Node (R cmd)
    (fmap (\ k x -> hClosure nv (k x)) ks)
    (fmap (\ k x -> hClosure (x:nv) (k x)) k)

hRecord (RECORD vs k) = do
  x <- malloc (length vs)
  zipWithM_ (\ i v -> store i x v) [0..] vs
  hRecord (k x)
hRecord (SELECT i v k) = do
  x <- load i v
  hRecord (k x)
hRecord (Leaf v) = Leaf v
hRecord (Node (R cmd) ks k) =
  Node cmd
    (fmap (\ k x -> hRecord (k x)) ks)
    (fmap (\ k x -> hRecord (k x)) k)

reify node x k = do
  x' <- node
  set x x'
  k

hFun xs (Leaf v) = ([],Leaf v)

hFun xs (APP v vs)  = ([],app v vs)
hFun (x:xs) (ADD v1 v2 k) = case hFun xs (k (VAR x)) of (fs,k) -> (fs, reify (add v1 v2) x k)
hFun xs (SET x v k) = case hFun xs (k (VAR x)) of (fs,k) -> (fs, do set x v; k)

hFun (x:xs) (MALLOC i k) = case hFun xs (k (VAR x)) of (fs,k) -> (fs, reify (malloc i) x k)
hFun (x:xs) (LOAD i v k) = case hFun xs (k (VAR x)) of (fs,k) -> (fs, reify (load i v) x k)
hFun (x:xs) (STORE i s t k) = case hFun xs (k (VAR x)) of (fs,k) -> (fs, reify (store i s t) x k)

hFun xs (FUN i b k) = let (as,f:xs') = splitAt i xs in
  case hFun xs' (k (LABEL f))              of { (fs,k) ->
  case hFun xs' (b (LABEL f : map VAR as)) of { (fs',b') ->
  ((f,as,b'):fs'++fs,k)}}
                                   
