{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trans.Hps2Wat where

import Val
import Types

import Data.Maybe
import Data.List

import Hps.Syntax
import Wat.Syntax

import qualified Hps.Syntax as H (Exp)
import qualified Wat.Syntax as W (Exp)

type FuncNames = [Var]

instance Transformable Hps Wat where
  transform (fs,e) = (map (fmap trans) fs,trans e)
    where ns = map (\ (f,as,b) -> f) fs
          trans e = transform e ns

instance Transformable H.Exp (FuncNames -> W.Exp) where
  transform (DONE v)         ns = Done (transform v ns)
  transform (APP v vs)       ns = App (transform v ns) (map (flip transform ns) vs)
  transform (ADD v1 v2 x e)  ns = Add (transform v1 ns) (transform v2 ns) x (transform e ns)
  transform (SELECT i v x e) ns = Load i (transform v ns) x (transform e ns)
  transform (RECORD vs x e)  ns = Malloc (length vs) x
    (foldr (\ (i,v) -> Store i (VAR x) (transform v ns)) (transform e ns) (zip [0..] vs))

instance Transformable Val (FuncNames -> Val) where
  transform (LABEL x) ns = INT (fromJust (x `elemIndex` ns))
  transform v         ns = v
