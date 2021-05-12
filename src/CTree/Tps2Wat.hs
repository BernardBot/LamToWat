{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module CTree.Tps2Wat where

import Data.List
import Data.Maybe

import Val
import Types

import CTree.Option
import CTree.Vec
import CTree.Union
import CTree.Commands (Malloc,Base,Empty)

import CTree.Tps
import Wat

import qualified CTree.Commands as T
import qualified Wat as W

type WatTps = Fix (Tps (Malloc :+: Base :+: Empty) Val)

tps2wat :: WatTps -> Wat
tps2wat (fs,e) = (map (fmap t2w) fs,t2w e)
  where ns = map (\ (f,as,b) -> f) fs

        t2w (Leaf v) = Val (v2v v)
        t2w (Node cmd ks k) = case (cmd,ks,k) of
          ((L (T.Malloc i)),      Nil, (Some (x,k))) -> W.Malloc i x (t2w k)
          ((L (T.Load i v)),      Nil, (Some (x,k))) -> W.Load i (v2v v) x (t2w k)
          ((L (T.Store i s t)),   Nil, (Some (_,k))) -> W.Store i (v2v s) (v2v t) (t2w k)
          ((R (L (T.Add v1 v2))), Nil, (Some (x,k))) -> W.Add (v2v v1) (v2v v2) x (t2w k)
          ((R (L (T.App v vs))),  Nil, None)         -> W.App (v2v v) (map v2v vs)

        v2v (LABEL x) = INT $ fromJust $ x `elemIndex` ns
        v2v v         = v

instance Emitable WatTps where
  emit = emit . tps2wat
