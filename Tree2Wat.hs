{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Tree2Wat where

import Tree
import Wat

import Data.List
import Data.Maybe

type Tree' = Tree (Malloc :+: Base) Tree.Val

tree2wat :: ([(String, [String], Tree')], Tree') -> Wat
tree2wat (fs,e) = Module fs' (go e)
  where ns :: [String]
        ns = map (\ (f,as,b) -> f) fs

        fs' :: [(String,[String],Exp)]
        fs' = map (\ (f,as,b) -> (f,as,go b)) fs

        go :: Tree (Malloc :+: Base) Tree.Val -> Exp
        go (Leaf v) = Done (vo v)
        go (APP v vs) = App (vo v) (map vo vs)
        go (ADD v1 v2 x k) = Add (vo v1) (vo v2) x (go (k (Tree.VAR x)))
        
        go (MALLOC i x k) = Malloc i x (go (k (Tree.VAR x)))
        go (LOAD i v x k) = Load i (vo v) x (go (k (Tree.VAR x)))
        go (STORE i s t k) = Store i (vo s) (vo t) (go (k (Tree.INT 0)))
        
        vo :: Tree.Val -> Wat.Val
        vo (Tree.INT i) = Wat.INT i
        vo (Tree.VAR x) = Wat.VAR x
        vo (Tree.LABEL x) = Wat.INT (fromJust (x `elemIndex` ns))
        

