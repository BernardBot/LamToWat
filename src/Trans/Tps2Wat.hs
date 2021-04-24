{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Trans.Tps2Wat where

import Data.List
import Data.Maybe

import Types

import Option
import Vec
import Union
import Commands hiding (Fix)

import qualified Commands as T

import qualified Tps.Syntax as T
import qualified Wat.Syntax as W

import Tps.Syntax
import Wat.Syntax

type Wat = Wat.Syntax.Expr
type WatTps = Tps (Malloc :+: Base :+: Empty) Val

instance Transformable (Fix WatTps) Wat where
  transform (fs,e) = (map (fmap trans) fs,trans e)
    where ns = map (\ (f,as,b) -> f) fs
          trans e = transform (ns,e)

instance Transformable ([Var],WatTps) W.Exp where
  transform (ns,hexp) = let trans e = transform (ns,e) in case hexp of
    Leaf v                                      -> W.Done (trans v)
    Node (L (T.Malloc i))      Nil (Some (x,k)) -> W.Malloc i x (transform (ns,k))
    Node (L (T.Load i v))      Nil (Some (x,k)) -> W.Load i (trans v) x (transform (ns,k))
    Node (L (T.Store i s t))   Nil (Some (_,k)) -> W.Store i (trans s) (trans t) (transform (ns,k))
    Node (R (L (T.Add v1 v2))) Nil (Some (x,k)) -> W.Add (trans v1) (trans v2) x (transform (ns,k))
    Node (R (L (T.App v vs)))  Nil None         -> W.App (trans v) (map trans vs)

instance Transformable ([Var],Val) Val where
  transform (ns,LABEL x) = INT (fromJust (x `elemIndex` ns))
  transform (ns,v)       = v
