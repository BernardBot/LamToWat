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
          [(String, Val)]
       -> Tree (Block :+: sig) Val -> Tree sig Val
hBlock nv (BLOCK b k) = do
  r <- freshlabel "r"
  x <- freshvar "x"
  fun r [x] (hBlock nv (k x))
  v <- hBlock (("_nxt",r):nv) b
  app r [v]
hBlock nv (GETK x k) | Just v <- lookup x nv = hBlock nv (k v)
hBlock nv (SETK x v k) = hBlock ((x,v):nv) (k (INT 0))
hBlock nv (Leaf v) = Leaf v
hBlock nv (Node (R cmd) ks k) =
  Node cmd
    (fmap (\ k -> hBlock nv k) ks)
    (fmap (\ k x -> hBlock nv (k x)) k)

hFresh :: Int -> [Int] -> Tree (Fresh :+: sig) Val -> Tree sig Val
hFresh i s (FRESHLABEL x k) =
  hFresh (i+1) s (k (LABEL ("_" ++ x ++ intercalate "_" (map show s) ++ "__" ++ show i)))
hFresh i s (FRESHVAR x k) =
  hFresh (i+1) s (k (VAR   ("_" ++ x ++ intercalate "_" (map show s) ++ "__" ++ show i)))
hFresh i s (Leaf v) = Leaf v
hFresh i s (Node (R cmd) ks k) = do
  Node cmd
    (zipWith (\ k j -> hFresh i (s ++ [j]) k) ks [1..]) -- hack?
    (fmap (\ k x -> hFresh i (s ++ [0])  (k x)) k)

_nv = VAR "_nv"
_p = ('_' :)

hClosure :: ( Fun :<: sig
            , Base :<: sig) =>
            [Val] -> Tree sig Val -> Tree (Record :+: sig) Val
hClosure nv (FUN f as b k) = do
  fun f (_nv:as)
    (do select 1 _nv _nv
        zipWithM_ (\ i v ->
                     if v `elem` as
                     then return v
                     else select i _nv v) [0..] nv
        hClosure (nv++as) b)
  hClosure nv (k f)
hClosure nv (APP v vs) = do
  record nv _nv
  vs' <- mapM (\ v -> case v of
                   LABEL f -> record [v,_nv] (VAR (_p f))
                   _       -> return v) vs
  case v of
    VAR f -> do
      fp <- select 0 v (VAR (_p f))
      app fp (v:vs')
    LABEL f -> do
      fc <- record [v,_nv] (VAR (_p f))
      app v (fc:vs')
hClosure nv (Leaf v) = Leaf v
hClosure nv (Node cmd ks k) =
  Node (R cmd)
    (fmap (\ k -> hClosure nv k) ks) -- hack
    (fmap (\ k x -> hClosure (nv++[x]) (k x)) k)

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
hFun (MALLOC i x k)  = case hFun (k x) of (fs,k') -> (fs, malloc i x >> k')
hFun (LOAD i v x k)  = case hFun (k x) of (fs,k') -> (fs, load i v x >> k')
hFun (STORE i s t k) = case hFun (k (INT 0)) of (fs,k') -> (fs, store i s t >> k')
hFun (FUN f as b k)  = case hFun b of (fs,b') -> case hFun (k f) of (fs',k') -> ((f,as,b'):fs'++fs,k')
