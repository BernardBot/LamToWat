module Cont.Cps2Wat where

import Data.List
import Data.Maybe

import Val

import Cont.Cps
import Wat

cps2wat :: Cps -> Wat
cps2wat (FIX fs e) = (map (fmap c2w) fs,c2w e)
    where ns = map (\ (f,as,b) -> f) fs

          c2w (DONE v)         = Done (v2v v)
          c2w (APP v vs)       = App (v2v v) (map v2v vs)
          c2w (ADD v1 v2 x e)  = Add (v2v v1) (v2v v2) x (c2w e)
          c2w (SELECT i v x e) = Load i (v2v v) x (c2w e)
          c2w (RECORD vs x e)  =
            Malloc (length vs) x $
            foldr (\ (i,v) -> Store i (VAR x) (v2v v))
              (c2w e) (zip [0..] vs)

          v2v (LABEL x) = INT $ fromJust $ x `elemIndex` ns
          v2v v         = v
