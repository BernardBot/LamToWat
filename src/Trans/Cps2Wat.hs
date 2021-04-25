module Trans.Cps2Wat where

import Val

import Data.Maybe
import Data.List

import Cps.Syntax
import Wat.Syntax

cps2wat :: Cps -> Wat
cps2wat (FIX fs e) = (map (fmap exp2wat) fs,exp2wat e)
    where ns = map (\ (f,as,b) -> f) fs

          exp2wat (DONE v)         = Done (val2wat v)
          exp2wat (APP v vs)       = App (val2wat v) (map val2wat vs)
          exp2wat (ADD v1 v2 x e)  = Add (val2wat v1) (val2wat v2) x (exp2wat e)
          exp2wat (SELECT i v x e) = Load i (val2wat v) x (exp2wat e)
          exp2wat (RECORD vs x e)  = Malloc (length vs) x
            (foldr (\ (i,v) -> Store i (VAR x) (val2wat v)) (exp2wat e) (zip [0..] vs))

          val2wat (LABEL x) = INT (fromJust (x `elemIndex` ns))
          val2wat v         = v
