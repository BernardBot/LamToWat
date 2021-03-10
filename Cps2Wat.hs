module Cps2Wat where

import Cps
import Wat

import Data.Maybe
import Data.List

cps2wat :: Cps -> Wat
cps2wat (FIX fs e) = Module fs' (go e)
  where ns :: [String]
        ns = map (\ (f,as,b) -> f) fs

        fs' :: [(String,[String],Exp)]
        fs' = map (\ (f,as,b) -> (f,as,go b)) fs

        go :: Cps -> Exp
        go (APP v vs) = App (vo v) (map vo vs)
        go (DONE v) = Done (vo v)
        go (RECORD vs x e) = Malloc (length vs) x
          (foldr (\ (i,v) -> Store i (Wat.VAR x) (vo v)) (go e) (zip [0..] vs))
        go (SELECT i v x e) = Load i (vo v) x (go e)
        go (ADD v1 v2 x e) = Add (vo v1) (vo v2) x (go e)

        vo :: Cps.Val -> Wat.Val
        vo (Cps.INT i) = Wat.INT i
        vo (Cps.VAR x) = Wat.VAR x
        vo (Cps.LABEL x) = Wat.INT (fromJust (x `elemIndex` ns))

