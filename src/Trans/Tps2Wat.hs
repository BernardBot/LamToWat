{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Trans.Tps2Wat where

import Data.List
import Data.Maybe

import Val
import Types

import Option
import Vec
import Union
import Commands hiding (Fix)

import qualified Commands as T
import qualified Wat.Syntax as W

import Tps.Syntax
import Wat.Syntax

type WatTps = Tps (Malloc :+: Base :+: Empty) Val

type FuncNames = [Var]

instance Transformable (Fix WatTps) Wat where
  transform (fs,e) = (map (fmap trans) fs,trans e)
    where ns = map (\ (f,as,b) -> f) fs
          trans e = transform e ns

instance Transformable WatTps (FuncNames -> W.Exp) where
  transform (Leaf v)                                      ns = W.Done (transform v ns)
  transform (Node (L (T.Malloc i))      Nil (Some (x,k))) ns = W.Malloc i x (transform k ns)
  transform (Node (L (T.Load i v))      Nil (Some (x,k))) ns = W.Load i (transform v ns) x (transform k ns)
  transform (Node (L (T.Store i s t))   Nil (Some (_,k))) ns = W.Store i (transform s ns) (transform t ns) (transform k ns)
  transform (Node (R (L (T.Add v1 v2))) Nil (Some (x,k))) ns = W.Add (transform v1 ns) (transform v2 ns) x (transform k ns)
  transform (Node (R (L (T.App v vs)))  Nil None)         ns = W.App (transform v ns) (map (flip transform ns) vs)

instance Transformable Val (FuncNames -> Val) where
  transform (LABEL x) ns = INT (fromJust (x `elemIndex` ns))
  transform v         ns = v
