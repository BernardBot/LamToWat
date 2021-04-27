{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module CTree.Tps2Tps where

import Types (Var)

import Data.Maybe

import Control.Monad

import Val
import qualified Types as T (Fix)

import CTree.Option
import CTree.Vec
import CTree.Union
import CTree.Commands

import CTree.Tps

tps2tps = hFix . swapTps . hRecord . hClos

hClos :: Tps            (Fix :+: Base :+: cmd) Val
      -> Tps (Record :+: Fix :+: Base :+: cmd) Val
hClos = hCl []

hCl :: [Var]
       -> Tps            (Fix :+: Base :+: cmd) Val
       -> Tps (Record :+: Fix :+: Base :+: cmd) Val
hCl nv (Node (L (Fix fxs)) bs (Some (_,k))) =
  fix' (mapV addArg fxs)
       (zipWithV funClos fxs bs)
       (hCl nv k)
  where addArg (name,args) = (name,"_closure":args)

        funClos (name,args) body = do
          select_ 1 (VAR "_closure") "_env"
          zipWithM_ (openClos args) [0..] nv
          hCl (nv++args) body

        openClos args i x =
          if x `elem` args then return () else select_ i (VAR "_env") x

hCl nv (Node (R (L (App fun args))) Nil None) = do
  record_ (map VAR nv) "_env"
  args' <- mapM mkClos args

  case fun of
    LABEL fp -> let cl = '_' : fp in do
      record_ [LABEL fp,VAR "_env"] cl
      app (LABEL fp) (VAR cl : args')

    VAR cl -> let fp = '_' : cl in do
      select_ 0 (VAR cl) fp
      app (VAR fp) (VAR cl : args')

  where mkClos (LABEL x) = let _x = '_' : x in do
            record_ [LABEL x,VAR "_env"] _x
            return $ VAR _x
        mkClos v = return v

hCl nv (Leaf v) = Leaf v
hCl nv (Node cmd ks k) =
  Node (R cmd)
    (fmap (hCl nv) ks)
    (fmap (\ (x,k) -> (x,hCl (extendnv nv x) k)) k)
  where extendnv nv "" = nv
        extendnv nv x = nv ++ [x]

hRecord :: Tps (Record :+: cmd) Val ->
           Tps (Malloc :+: cmd) Val
hRecord (Node (L (Record vs)) Nil (Some (x,k))) = do
  malloc_ (length vs) x
  zipWithM_ (\ i -> store_ i (VAR x)) [0..] vs
  hRecord k

hRecord (Node (L (Select i v)) Nil (Some (x,k))) =
  load i v x (hRecord k)

hRecord (Leaf v) = Leaf v
hRecord (Node (R cmd) ks k) =
  Node (R cmd)
    (fmap hRecord ks)
    (fmap (fmap hRecord) k)

hFix :: Tps (Fix :+: cmd) Val ->
        T.Fix (Tps cmd Val)
hFix (Leaf v) = ([],Leaf v)
hFix (Node (R cmd) ks k) = case k of

  Some (x,k) -> (fs++fs',Node cmd ks' (Some (x,k')))
    where (fs,k') = hFix k

  None -> (fs',Node cmd ks' None)

  where ks' = mapV (snd . hFix) ks
        fs' = concatMap (fst . hFix) $ toList ks

hFix (Node (L (Fix fxs)) bs (Some ("",k))) = (fs'++fs,k')
  where fs' = concat $ zipWith hFun (toList fxs) (toList bs)
        (fs,k') = hFix k

        hFun (f,as) b = (f,as,b') : fs
          where (fs,b') = hFix b
