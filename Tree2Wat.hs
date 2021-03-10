{-# LANGUAGE GADTs #-}

module Tree2Wat where

import Tree
import Wat

import Data.List
import Data.Maybe

tree2wat (fs,e) = Module fs' (go e)
  where ns :: [String]
        ns = map (\ (f,as,b) -> f) fs

        fs' :: [(String,[String],Exp)]
        fs' = map (\ (f,as,b) -> (f,as,go b)) fs

        go (Leaf v) = Done (vo v)
        go (APP v vs) = App (vo v) (map vo vs)
        go (SET x v k) = Set x (vo v) (go (k (Tree.VAR x)))
        go (ADD v1 v2 k) = Add (vo v1) (vo v2) "x" (go (k (Tree.VAR "x")))

        go (MALLOC i k) = Malloc i "x" (go (k (Tree.VAR "x")))
        go (LOAD i v k) = Load i (vo v) "x" (go (k (Tree.VAR "x")))
        go (STORE i s t k) = Store i (vo s) (vo t) (go (k (Tree.VAR "x")))

        vo :: Tree.Val -> Wat.Val
        vo (Tree.INT i) = Wat.INT i
        vo (Tree.VAR x) = Wat.VAR x
        vo (Tree.LABEL x) = Wat.INT (fromJust (x `elemIndex` ns))
        

