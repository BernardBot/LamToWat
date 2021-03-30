{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tps where

import Val
import Option
import Vec

import Control.Monad

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

---------------
-- Injection --
---------------

data (:+:) :: Sig -> Sig -> Sig where
  L :: sigl n b -> (sigl :+: sigr) n b
  R :: sigr n b -> (sigl :+: sigr) n b
infixr 7 :+:

infixr 6 :<:
class (sub :: Sig) :<: (sup :: Sig) where
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

liftSigF :: (forall n b. sig n b -> sig' n b) -> Tps sig Val -> Tps sig' Val
liftSigF f tree = go tree
  where go (Leaf v) = Leaf v
        go (Node cmd ks k) = Node (f cmd) (fmap go ks) (fmap (fmap go) k)
                       
swap' :: (f :+: g :+: h) n b -> (g :+: f :+: h) n b
swap' (L a)     = R (L a)
swap' (R (L a)) = L a
swap' (R (R a)) = R (R a)

swap :: Tps (f :+: g :+: h) Val -> Tps (g :+: f :+: h) Val
swap = liftSigF swap'

merge' :: (f :+: f :+: g) n b -> (f :+: g) n b
merge' (L a)     = L a
merge' (R (L a)) = L a
merge' (R (R a)) = R a

merge :: Tps (f :+: f :+: g) Val -> Tps (f :+: g) Val
merge = liftSigF merge'

---------------------
-- Pretty-Printing --
---------------------

-- TODO

data Print :: Sig where
  Print :: String -> Print Z True

pBase :: Tps (Base :+: cmd) a -> Tps (Base :+: cmd) a
pBase = undefined

pRecord :: Tps (Record :+: cmd) a -> Tps (Print :+: cmd) a
pRecord = undefined

pFix :: Tps (Fix :+: cmd) a -> Tps (Print :+: cmd) a -- do it last?
pFix = undefined

pMalloc :: Tps (Malloc :+: cmd) a -> Tps (Print :+: cmd) a
pMalloc = undefined

instance Show a => Show (Tps (Print :+: VoidCmd) a) where
  show (Leaf a) = show a
  show (Node (L (Print s)) Nil (Some (_,k))) = s ++ show k

e = 
    Node (L (Fix (("f", ["x"]) ::: Nil)))
    ((Node (R (Add (VAR "x") (INT 1))) Nil (Some ("x", Leaf (VAR "x")))) ::: Nil) (Some ("",
    Node (R (App (VAR "f") [INT 41])) Nil None))

e' :: Tps (Fix :+: Base) Val
e' = do
  fix_ (("f",["x"],do
        add_ (VAR "x") (INT 1) "n"
        done (VAR "n"))
        ::: Nil)
  app (VAR "f") [INT 41]
