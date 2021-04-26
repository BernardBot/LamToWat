module Cont.Cps2Wat where

import Types (Var)

import Val

import Cont.Cps
import Wat

cps2wat :: Cps -> Wat
cps2wat (FIX fs e) = (map (fmap c2w) fs,c2w e)
    where ns :: [Var]
          ns = map (\ (f,as,b) -> f) fs

          cv2wv :: Val -> Val
          cv2wv = flip pointify ns -- Cps to Wat Val

          c2w (DONE v)         = Done (cv2wv v)
          c2w (APP v vs)       = App (cv2wv v) (map cv2wv vs)
          c2w (ADD v1 v2 x e)  = Add (cv2wv v1) (cv2wv v2) x (c2w e)
          c2w (SELECT i v x e) = Load i (cv2wv v) x (c2w e)
          c2w (RECORD vs x e)  =
            Malloc (length vs) x $
            foldr (\ (i,v) -> Store i (VAR x) (cv2wv v))
              (c2w e) (zip [0..] vs)
