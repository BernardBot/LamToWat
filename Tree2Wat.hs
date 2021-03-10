{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Tree2Wat where

import Tree
import Wat

import Data.List
import Data.Maybe

tree2wat (fs,e) = Module fs' (go 0 e)
  where ns :: [String]
        ns = map (\ (f,as,b) -> f) fs

        fs' :: [(String,[String],Exp)]
        fs' = map (\ (f,as,b) -> (f,as,go 0 b)) fs

        go :: Int -> Tree (Malloc :+: Base) Tree.Val -> Exp
        go n tree = let y = "_y" ++ show n; n' = n + 1; w = Tree.VAR y in case tree of
          Leaf v -> Done (vo v)

          APP v vs    -> App   (vo v) (map vo vs)

          ADD v1 v2 k -> Add      (vo v1) (vo v2) y (go n' (k w))
          MALLOC i  k -> Malloc i                 y (go n' (k w))
          LOAD i v  k -> Load   i  (vo v)         y (go n' (k w))

          SET x v     k -> Set x (vo v)          (go n (k undefined))
          STORE i s t k -> Store i (vo s) (vo t) (go n (k undefined))

        vo :: Tree.Val -> Wat.Val
        vo (Tree.INT i) = Wat.INT i
        vo (Tree.VAR x) = Wat.VAR x
        vo (Tree.LABEL x) = Wat.INT (fromJust (x `elemIndex` ns))
        

