{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tps.PPrinter where

import Option
import Vec

import Tps.Syntax
import Tps.Commands
import Tps.Union

import Types hiding (Record,Fix)

type TpsWat = Tps (Malloc :+: Base :+: VoidCmd) Val

instance {-# OVERLAPPING #-} Show ([(String,[String],TpsWat)],TpsWat) where
  show (fs,e) = concatMap (\ (f,as,b) -> "def " ++ f ++ args as ++ ":\n" ++ indent (show b)) fs ++ show e

instance Show (Base n b) where
  show (App v vs) = show v ++ args (map show vs)
  show (Add v1 v2) = show v1 ++ " + " ++ show v2

instance Show (Record n b) where
  show (Record vs) = show vs
  show (Select i v) = show v ++ "[" ++ show i ++ "]"

instance Show (Malloc n b) where
  show (Malloc i) = "malloc " ++ show i
  show (Load i v) = "load " ++ show i ++ " " ++ show v
  show (Store i s t) = "store " ++ show i ++ " " ++ show s ++ " " ++ show t

data Print :: Sig where
  Print :: (Vec n String -> String) -> Print n b

class Printable (f :: Sig) where
  pCmd :: f n b -> Print n b

  pTps :: Tps f a -> Tps Print a
  pTps (Leaf a) = Leaf a
  pTps (Node cmd ks k) = Node (pCmd cmd) (fmap pTps ks) (fmap (fmap pTps) k)

  pTps' :: Tps (f :+: cmd) a -> Tps (Print :+: cmd) a
  pTps' (Leaf a) = Leaf a
  pTps' (Node (L cmd) ks k) = Node (L $ pCmd cmd) (fmap pTps' ks) (fmap (fmap pTps') k)
  pTps' (Node (R cmd) ks k) = Node (R cmd)        (fmap pTps' ks) (fmap (fmap pTps') k)

instance Printable Base where
  pCmd cmd = Print $ const $ show cmd
instance Printable Record where
  pCmd cmd = Print $ const $ show cmd
instance Printable Malloc where
  pCmd cmd = Print $ const $ show cmd
instance Printable Fix where
  pCmd (Fix Nil) = Print $ \ Nil -> "def:"
  pCmd (Fix fxs) = Print $ \ bs ->
    init $ concat $ toList $ zipWithV (\ (f,as) b -> "def " ++ f ++ args as ++ ":\n" ++ indent b) fxs bs

instance {-# OVERLAPPING #-} Show a => Show (Tps Print a) where
  show (Leaf a) = "return " ++ show a
  show (Node (Print f) ks k) = let s = f (mapV show ks) in case k of
    Some ("",k) -> s          ++ "\n" ++ show k
    Some (x ,k) -> assign x s ++         show k
    None        -> s
instance                     (Show a, Printable f)                       => Show (Tps f                     a) where show = show . pTps
instance {-# OVERLAPPING #-} (Show a, Printable f)                       => Show (Tps (Print :+: f)         a) where show = show . morge . pTps' . swop
instance {-# OVERLAPPING #-} Show a                                      => Show (Tps (Print :+: VoidCmd)   a) where show = show . dropVoid
instance {-# OVERLAPPING #-} (Show (Tps (Print :+: cmd) a), Printable f) => Show (Tps (f :+: cmd)           a) where show = show . pTps'
instance {-# OVERLAPPING #-} (Show (Tps (Print :+: cmd) a), Printable f) => Show (Tps (Print :+: f :+: cmd) a) where show = show . merge . pTps' . swap

----------------------
-- Helper Functions --
----------------------

liftSigF :: (forall n b. sig n b -> sig' n b) -> Tps sig a -> Tps sig' a
liftSigF f (Leaf v)        = Leaf v
liftSigF f (Node cmd ks k) = Node (f cmd) (fmap (liftSigF f) ks) (fmap (fmap (liftSigF f)) k)
                       
swap' :: (f :+: g :+: h) n b -> (g :+: f :+: h) n b
swap' (L a)     = R (L a)
swap' (R (L a)) = L a
swap' (R (R a)) = R (R a)

swap :: Tps (f :+: g :+: h) a -> Tps (g :+: f :+: h) a
swap = liftSigF swap'

merge' :: (f :+: f :+: g) n b -> (f :+: g) n b
merge' (L a)     = L a
merge' (R (L a)) = L a
merge' (R (R a)) = R a

merge :: Tps (f :+: f :+: g) a -> Tps (f :+: g) a
merge = liftSigF merge'

dropVoid' :: (f :+: VoidCmd) n b -> f n b
dropVoid' (L a) = a

dropVoid = liftSigF dropVoid'

addVoid' :: f n b -> (f :+: VoidCmd) n b
addVoid' a = (L a)

addVoid = liftSigF addVoid'

swop' (L a) = R a
swop' (R a) = L a

swop = liftSigF swop'

morge' (L a) = a
morge' (R a) = a

morge = liftSigF morge'


-- Test --

e :: Tps (Fix :+: Base) a
e = 
    Node (L (Fix (("f", ["x"]) ::: Nil)))
    ((Node (R (Add (VAR "x") (INT 1))) Nil (Some ("n", Leaf (VAR "n")))) ::: Nil) (Some ("",
    Node (R (App (VAR "f") [INT 41])) Nil None))

e' :: Tps (Malloc :+: Fix :+: Base :+: VoidCmd) Val
e' = do
  fix_ (("f",["x"],do
        add_ (VAR "x") (INT 1) "n"
        done (VAR "n"))
        ::: Nil)
  malloc_ 0 "x"
  app (VAR "f") [INT 41]

e'' :: Tps Fix ()
e'' = fix_ Nil
