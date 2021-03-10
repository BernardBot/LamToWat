{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Tree2Tree where

import Tree

import Data.List
import Control.Monad

hBlock :: [(String,Val)] -> Tree (Block :+: Fun :+: Base) Val -> Tree (Fun :+: Base) Val
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

hClosure :: [Val] -> Tree (Fun :+: Base) Val -> Tree (Record :+: Fun :+: Base) Val
hClosure nv (FUN i b k) = do
  f <- fun (i+1) (\ (f:c:as) -> do
                _c <- select 1 c
                zipWithM_
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
hClosure nv (SET x v k) = do set x v; hClosure nv (k undefined)
hClosure nv (Leaf v) = Leaf v
hClosure nv (Node cmd ks k) =
  Node (R cmd)
    (fmap (\ k x -> hClosure nv (k x)) ks)
    (fmap (\ k x -> hClosure (x:nv) (k x)) k)

hRecord :: Tree (Record :+: sig) Val -> Tree (Malloc :+: sig) Val
hRecord (RECORD vs k) = do
  x <- malloc (length vs)
  zipWithM_ (\ i v -> store i x v) [0..] vs
  hRecord (k x)
hRecord (SELECT i v k) = do
  x <- load i v
  hRecord (k x)
hRecord (Leaf v) = Leaf v
hRecord (Node (R cmd) ks k) =
  Node (R cmd)
    (fmap (\ k x -> hRecord (k x)) ks)
    (fmap (\ k x -> hRecord (k x)) k)

type T = Tree (Malloc :+: Base) Val

hFun :: Int -> Tree (Malloc :+: Fun :+: Base) Val -> ([(String,[String],T)], T)
hFun i (Leaf v) = ([],Leaf v)
hFun i (APP v vs)  = ([],app v vs)

hFun i (ADD v1 v2 k)   = let x = "_x" ++ show i; i' = i + 1 in case hFun i' (k (VAR x)) of (fs,k) -> (fs, reify (add v1 v2) x k)
hFun i (MALLOC j k)    = let x = "_x" ++ show i; i' = i + 1 in case hFun i' (k (VAR x)) of (fs,k) -> (fs, reify (malloc j) x k)
hFun i (LOAD j v k)    = let x = "_x" ++ show i; i' = i + 1 in case hFun i' (k (VAR x)) of (fs,k) -> (fs, reify (load j v) x k)

hFun i (SET x v k)     = case hFun i (k undefined) of (fs,k) -> (fs, do set x v; k)
hFun i (STORE j s t k) = case hFun i (k undefined) of (fs,k) -> (fs, do store j s t; k)

hFun i (FUN j b k)     = let (as,[f]) = splitAt j (map (("_x"++) . show) [i..i+j]); i' = i + j + 1 in
  case hFun i' (k (LABEL f))              of { (fs,k) ->
  case hFun i' (b (LABEL f : map VAR as)) of { (fs',b') ->
  ((f,as,b'):fs'++fs,k)}}
                                   
reify node x k = do
  x' <- node
  set x x'
  k
