{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tps where

import Types hiding (fix,Fix,record,Record,malloc,load,store)
import Option
import Vec

import Control.Monad

import Data.List

data Tps sig a where
  Leaf :: a -> Tps sig a
  Node :: sig n b
       -> Vec n (Tps sig Val)
       -> Option b (String, Tps sig a)
       -> Tps sig a

instance Monad (Tps sig) where
  Leaf x       >>= g = g x
  Node op cs k >>= g = Node op cs (fmap (\ (x,k) -> (x, k >>= g)) k)

instance Functor (Tps sig) where
  fmap = liftM

instance Applicative (Tps sig) where
  pure = Leaf
  (<*>) = ap

liftT op ps x k = Node op ps (Some (x, k))
liftF op ps = Node op ps None

------------------
-- CPS Commands --
------------------

type Sig = Nat -> Bool -> *

data Base :: Sig where
  App :: Val -> [Val] -> Base Z False
  Add :: Val -> Val ->   Base Z True 

data Record :: Sig where
  Record :: [Val] ->      Record Z True
  Select :: Int -> Val -> Record Z True

data Fix :: Sig where
  Fix :: Vec n (String,[String]) -> Fix n True

data Malloc :: Sig where
  Malloc :: Int ->               Malloc Z True
  Load   :: Int -> Val ->        Malloc Z True
  Store  :: Int -> Val -> Val -> Malloc Z True

data VoidCmd :: Sig where

-- used for converting to Wat
type TpsWat = Tps (Malloc :+: Base :+: VoidCmd) Val
---------------
-- Injection --
---------------

data (:+:) :: Sig -> Sig -> Sig where
  L :: sigl n b -> (sigl :+: sigr) n b
  R :: sigr n b -> (sigl :+: sigr) n b
infixr 7 :+:

infixr 6 :<:
class (:<:) (sub :: Sig) (sup :: Sig) where
  inj :: sub n b -> sup n b

instance a :<: a where
  inj = id

instance a :<: a :+: b where
  inj = L

instance {-# OVERLAPPABLE #-} a :<: c => a :<: b :+: c where
  inj = R . inj

------------------------
-- Smart Constructors --
------------------------
done = Leaf
app v vs = liftF (inj (App v vs)) Nil

add v1 v2 x k = liftT (inj (Add v1 v2)) Nil x k
add_ v1 v2 x = add v1 v2 x (Leaf ())

record vs x k = liftT (inj (Record vs)) Nil x k
record_ vs x = record vs x (Leaf ())

select i v x k = liftT (inj (Select i v)) Nil x k
select_ i v x = select i v x (Leaf ())

data VPair a b n = VPair { getVPair :: (Vec n a, Vec n b) }

fix fs k =
  let VPair (fxs, bs) =
        ifoldV (VPair (Nil, Nil))
               (\ (f, xs, b) (VPair (fxs, bs)) ->
                  VPair ((f, xs) ::: fxs, b ::: bs))
               fs
  in liftT (inj (Fix fxs)) bs "" k

fix_ fs = fix fs (Leaf ())

fix' fxs bs k = liftT (inj (Fix fxs)) bs "" k
fix'_ fxs bs = fix' fxs bs (Leaf ())

malloc i x k = liftT (inj (Malloc i)) Nil x k
malloc_ i x = malloc i x (Leaf ())

load i v x k = liftT (inj (Load i v)) Nil x k
load_ i v x = load i v x (Leaf ())

store i s t k = liftT (inj (Store i s t)) Nil "" k
store_ i s t = store i s t (Leaf ())

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

---------------------
-- Pretty-Printing --
---------------------

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
