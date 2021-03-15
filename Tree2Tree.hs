{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Tree2Tree where

import Tree

import Control.Monad

import Data.List

hBlock :: ( Fresh :<: sig
          , Fun :<: sig
          , Base :<: sig) =>
          Tree (Block :+: sig) Val -> Tree sig Val
hBlock = hBlock' []

hBlock' :: ( Fresh :<: sig
          , Fun :<: sig
          , Base :<: sig) =>
          [(String, Val)]
       -> Tree (Block :+: sig) Val -> Tree sig Val
hBlock' nv (BLOCK b k) = do
  VAR r <- fresh "r"
  VAR x <- fresh "x"
  fun r [x] (hBlock' nv (k (VAR x)))
  v <- hBlock' (("_nxt",LABEL r):nv) b
  app (VAR r) [v]
hBlock' nv (GETK x k) | Just v <- lookup x nv = hBlock' nv (k v)
hBlock' nv (SETK x v k) = hBlock' ((x,v):nv) (k (INT 0))
hBlock' nv (Leaf v) = Leaf v
hBlock' nv (Node (R cmd) ks k) =
  Node cmd
    (fmap (\ k -> hBlock' nv k) ks)
    (fmap (\ k x -> hBlock' nv (k x)) k)

hFresh :: Tree (Fresh :+: sig) Val -> Tree sig Val
hFresh = hFresh' 0 []

hFresh' :: Int -> [Int] -> Tree (Fresh :+: sig) Val -> Tree sig Val
hFresh' i s (FRESH x k) =
  hFresh' (i+1) s (k (VAR ("_" ++ x ++ intercalate "_" (map show s) ++ "__" ++ show i)))
hFresh' i s (Leaf v) = Leaf v
hFresh' i s (Node (R cmd) ks k) = do
  Node cmd
    (zipWith (\ k j -> hFresh' i (s ++ [j]) k) ks [1..]) -- hack?
    (fmap (\ k x -> hFresh' i (s ++ [0])  (k x)) k)

_nv = "_nv"
_p = ('_' :)

hClosure :: ( Fun :<: sig
            , Base :<: sig) =>
            Tree sig Val -> Tree (Record :+: sig) Val
hClosure = hClosure' []

hClosure' :: ( Fun :<: sig
            , Base :<: sig) =>
            [String] -> Tree sig Val -> Tree (Record :+: sig) Val
hClosure' nv (FUN f as b k) = do
  fun f (_nv:as)
    (do select 1 (VAR _nv) _nv
        zipWithM_ (\ i x ->
                     if x `elem` as
                     then return (VAR x)
                     else select i (VAR _nv) x) [0..] nv
        hClosure' (nv ++ as) b)
  hClosure' nv (k (LABEL f))
hClosure' nv (APP v vs) = do
  record (map VAR nv) _nv
  vs' <- mapM (\ v -> case v of
                   LABEL f -> record [v,VAR _nv] (_p f)
                   _       -> return v) vs
  case v of
    VAR f -> do
      fp <- select 0 v (_p f)
      app fp (v:vs')
    LABEL f -> do
      fc <- record [v,VAR _nv] (_p f)
      app v (fc:vs')
hClosure' nv (Leaf v) = Leaf v
hClosure' nv (Node cmd ks k) =
  Node (R cmd)
    (fmap (\ k -> hClosure' nv k) ks) -- hack
    (fmap (\ k v -> hClosure' (nv ++ case v of VAR x -> [x]; _ -> []) (k v)) k)

hRecord :: Tree (Record :+: sig) Val -> Tree (Malloc :+: sig) Val
hRecord (RECORD vs x k) = do
  malloc (length vs) x
  zipWithM_ (\ i v -> store i (VAR x) v) [0..] vs
  hRecord (k (VAR x))
hRecord (SELECT i v x k) = do
  load i v x
  hRecord (k (VAR x))
hRecord (Leaf v) = Leaf v
hRecord (Node (R cmd) ks k) =
  Node (R cmd)
    (fmap (\ k -> hRecord k) ks)
    (fmap (\ k x -> hRecord (k x)) k)

type Tree' = Tree (Malloc :+: Base) Val

hFun :: Tree (Malloc :+: Fun :+: Base) Val -> ([(String,[String],Tree')], Tree')
hFun (Leaf v) = ([],Leaf v)
hFun (APP v vs) = ([],app v vs)
hFun (ADD v1 v2 x k) = case hFun (k (VAR x)) of (fs,k') -> (fs, add v1 v2 x >> k')
hFun (MALLOC i x k)  = case hFun (k (VAR x)) of (fs,k') -> (fs, malloc i x >> k')
hFun (LOAD i v x k)  = case hFun (k (VAR x)) of (fs,k') -> (fs, load i v x >> k')
hFun (STORE i s t k) = case hFun (k (INT 0)) of (fs,k') -> (fs, store i s t >> k')
hFun (FUN f as b k)  = case hFun b of (fs,b') -> case hFun (k (LABEL f)) of (fs',k') -> ((f,as,b'):fs'++fs,k')
