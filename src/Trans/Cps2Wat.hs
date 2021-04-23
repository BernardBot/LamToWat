module Trans.Cps2Wat where

import Types (Val(INT,VAR,LABEL))
import Cps.Syntax
import Wat.Syntax

import Data.Maybe
import Data.List

type Cps = Cps.Syntax.Expr
type Wat = Wat.Syntax.Expr

cps2wat :: Cps -> Wat
cps2wat (FIX fs e) = (fs',(go e))
  where ns :: [String]
        ns = map (\ (f,as,b) -> f) fs

        fs' :: [(String,[String],Exp)]
        fs' = map (\ (f,as,b) -> (f,as,go b)) fs

        go :: Cps -> Exp
        go (APP v vs) = App (vo v) (map vo vs)
        go (DONE v) = Done (vo v)
        go (RECORD vs x e) = Malloc (length vs) x
          (foldr (\ (i,v) -> Store i (VAR x) (vo v)) (go e) (zip [0..] vs))
        go (SELECT i v x e) = Load i (vo v) x (go e)
        go (ADD v1 v2 x e) = Add (vo v1) (vo v2) x (go e)

        vo v = case v of
          LABEL x -> INT (fromJust (x `elemIndex` ns))
          _       -> v
