{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trans.Hps2Wat where

import Types

import Data.Maybe
import Data.List

import Hps.Syntax
import Wat.Syntax

import qualified Hps.Syntax as H (Exp)
import qualified Wat.Syntax as W (Exp)

type Hps = Hps.Syntax.Expr
type Wat = Wat.Syntax.Expr

instance Transformable Hps Wat where
  transform (fs,e) = (map (fmap trans) fs,trans e)
    where ns = map (\ (f,as,b) -> f) fs
          trans e = transform (ns,e)

instance Transformable ([Var],H.Exp) W.Exp where
  transform (ns,hexp) = let trans e = transform (ns,e) in case hexp of
    DONE v         -> Done (trans v)
    APP v vs       -> App (trans v) (map trans vs)
    ADD v1 v2 x e  -> Add (trans v1) (trans v2) x (trans e)
    SELECT i v x e -> Load i (trans v) x (trans e)
    RECORD vs x e  -> Malloc (length vs) x
      (foldr (\ (i,v) -> Store i (VAR x) (trans v)) (trans e) (zip [0..] vs))

instance Transformable ([Var],Val) Val where
  transform (ns,LABEL x) = INT (fromJust (x `elemIndex` ns))
  transform (ns,v)       = v
