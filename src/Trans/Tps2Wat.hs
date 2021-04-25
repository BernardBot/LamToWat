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

tps2wat :: Fix WatTps -> Wat
tps2wat (fs,e) = (map (fmap tps2wat') fs,tps2wat' e)
    where ns = map (\ (f,as,b) -> f) fs

          tps2wat' (Leaf v)                                      = W.Done (val2wat v)
          tps2wat' (Node (L (T.Malloc i))      Nil (Some (x,k))) = W.Malloc i x (tps2wat' k)
          tps2wat' (Node (L (T.Load i v))      Nil (Some (x,k))) = W.Load i (val2wat v) x (tps2wat' k)
          tps2wat' (Node (L (T.Store i s t))   Nil (Some (_,k))) = W.Store i (val2wat s) (val2wat t) (tps2wat' k)
          tps2wat' (Node (R (L (T.Add v1 v2))) Nil (Some (x,k))) = W.Add (val2wat v1) (val2wat v2) x (tps2wat' k)
          tps2wat' (Node (R (L (T.App v vs)))  Nil None)         = W.App (val2wat v) (map val2wat vs)

          val2wat (LABEL x) = INT (fromJust (x `elemIndex` ns))
          val2wat v         = v
