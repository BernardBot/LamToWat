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

import Tps.Syntax
import qualified Wat.Syntax as W

type Wat = W.Expr
type TpsWat = Tps (Malloc :+: Base :+: Empty) Val

tps2wat :: Fix TpsWat -> Wat
tps2wat (fs,e) = (fs',(go e))
  where ns :: [Var]
        ns = map (\ (f,as,b) -> f) fs

        fs' :: [(String,[String],W.Exp)]
        fs' = map (\ (f,as,b) -> (f,as,go b)) fs

        go :: TpsWat -> W.Exp
        go (Leaf v)                                    = W.Done (vo v)
        go (Node (L (Malloc i))      Nil (Some (x,k))) = W.Malloc i x (go k)
        go (Node (L (Load i v))      Nil (Some (x,k))) = W.Load i (vo v) x (go k)
        go (Node (L (Store i s t))   Nil (Some (_,k))) = W.Store i (vo s) (vo t) (go k)
        go (Node (R (L (Add v1 v2))) Nil (Some (x,k))) = W.Add (vo v1) (vo v2) x (go k)
        go (Node (R (L (App v vs)))  Nil None)         = W.App (vo v) (map vo vs)
        
        vo v = case v of
          LABEL x -> INT (fromJust (x `elemIndex` ns))
          _       -> v
