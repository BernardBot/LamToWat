{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Tps2Wat where

import Val
import Tps
import Wat (Wat(Module),Exp)
import qualified Wat as W
import Option
import Vec

import Data.List
import Data.Maybe

tps2wat :: ([(String,[String],TpsWat)],TpsWat) -> Wat
tps2wat (fs,e) = Module fs' (go e)
  where ns :: [String]
        ns = map (\ (f,as,b) -> f) fs

        fs' :: [(String,[String],Exp)]
        fs' = map (\ (f,as,b) -> (f,as,go b)) fs

        go :: TpsWat -> Exp
        go (Leaf v)                                    = W.Done (vo v)
        go (Node (L (Malloc i))      Nil (Some (x,k))) = W.Malloc i x (go k)
        go (Node (L (Load i v))      Nil (Some (x,k))) = W.Load i (vo v) x (go k)
        go (Node (L (Store i s t))   Nil (Some (_,k))) = W.Store i (vo s) (vo t) (go k)
        go (Node (R (L (Add v1 v2))) Nil (Some (x,k))) = W.Add (vo v1) (vo v2) x (go k)
        go (Node (R (L (App v vs)))  Nil None)         = W.App (vo v) (map vo vs)
        
        vo v = case v of
          LABEL x -> INT (fromJust (x `elemIndex` ns))
          _       -> v
