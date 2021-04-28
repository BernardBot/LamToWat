module Cont.Cps2Wat where

import Data.List
import Data.Maybe

import Val

import Cont.Cps
import Wat

import qualified Cont.Cps as C
import qualified Wat as W

cps2wat :: Cps -> Wat
cps2wat (C.Fix fs e) = (map (fmap c2w) fs,c2w e)
    where ns = map (\ (f,as,b) -> f) fs

          c2w (C.Val v)          = W.Val (v2v v)
          c2w (C.App v vs)       = W.App (v2v v) (map v2v vs)
          c2w (C.Add v1 v2 x e)  = W.Add (v2v v1) (v2v v2) x (c2w e)
          c2w (C.Select i v x e) = W.Load i (v2v v) x (c2w e)
          c2w (C.Record vs x e)  =
            W.Malloc (length vs) x $
            foldr (\ (i,v) -> W.Store i (VAR x) (v2v v))
              (c2w e) (zip [0..] vs)

          v2v (LABEL x) = INT $ fromJust $ x `elemIndex` ns
          v2v v         = v
