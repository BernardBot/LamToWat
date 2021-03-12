{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Tree2Wat where

import Tree
import Wat

import Data.List
import Data.Maybe

type Tree' = Tree (Malloc :+: Base) Tree.Val

tree2wat :: ([(Tree.Val, [Tree.Val], Tree')], Tree') -> Wat
tree2wat (fs,e) = Module fs' (go e)
  where ns :: [String]
        ns = map (\ (LABEL f,as,b) -> f) fs

        unVar (Tree.VAR x) = x

        fs' :: [(String,[String],Exp)]
        fs' = map (\ (LABEL f,as,b) -> (f,map unVar as,go b)) fs

        go :: Tree (Malloc :+: Base) Tree.Val -> Exp
        go (Leaf v) = Done (vo v)
        go (APP v vs) = App (vo v) (map vo vs)
        go (ADD v1 v2 v@(Tree.VAR x) k) = Add (vo v1) (vo v2) x (go (k v))
        
        go (MALLOC i v@(Tree.VAR x) k) = Malloc i x (go (k v))
        go (LOAD i v w@(Tree.VAR x) k) = Load i (vo v) x (go (k w))
        go (STORE i s t k) = Store i (vo s) (vo t) (go (k UNIT))
        
        vo :: Tree.Val -> Wat.Val
        vo (Tree.INT i) = Wat.INT i
        vo (Tree.VAR x) = Wat.VAR x
        vo (Tree.LABEL x) = Wat.INT (fromJust (x `elemIndex` ns))
        

