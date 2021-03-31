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

import Val
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

tab = "  "
indent = unlines . map (tab++) . lines
assign x y = x ++ " = " ++ y ++ "\n"
args ss = "(" ++ intercalate "," ss ++ ")"

data Print :: Sig where
  Print :: String -> Print Z True
  PrintWith :: (Vec n String -> String) -> Print n True
  Done :: String -> Print Z False

class Printable (f :: Sig) where
  pCmd :: f n b -> String -> Print n b

  pCmdT :: Tps f a -> Tps Print a
  pCmdT (Leaf a) = Leaf a
  pCmdT (Node cmd ks (Some (x,k))) = Node (pCmd cmd x)  (fmap pCmdT ks) (Some (x,pCmdT k))
  pCmdT (Node cmd ks None)         = Node (pCmd cmd "") (fmap pCmdT ks) None

  pCmdT' :: Tps (f :+: cmd) a -> Tps (Print :+: cmd) a
  pCmdT' (Leaf a) = Leaf a
  pCmdT' (Node (L cmd) ks (Some (x,k))) = Node (L $ pCmd cmd x)  (fmap pCmdT' ks) (Some (x,pCmdT' k))
  pCmdT' (Node (L cmd) ks None)         = Node (L $ pCmd cmd "") (fmap pCmdT' ks) None
  pCmdT' (Node (R cmd) ks k)            = Node (R cmd)           (fmap pCmdT' ks) (fmap (fmap pCmdT') k)

instance Printable Base where
  pCmd (App v vs) x = Done $ show v ++ args (map show vs)
  pCmd (Add v1 v2) x = Print $ assign x (show v1 ++ " + " ++ show v2)

instance Printable Record where
  pCmd (Record vs) x = Print $ assign x (show vs)
  pCmd (Select i v) x = Print $ assign x (show v ++ "[" ++ show i ++ "]")

instance Printable Fix where
  pCmd (Fix fxs) x = PrintWith $ \ bs -> concatMap (\ ((f,as),b) -> "def " ++ f ++ args as ++ ":\n" ++ indent b) (zip (toList fxs) (toList bs))

instance Printable Malloc where
  pCmd (Malloc i) x = Print $ assign x ("malloc " ++ show i)
  pCmd (Load i v) x = Print $ assign x ("load " ++ show i ++ " " ++ show v)
  pCmd (Store i s t) _ = Print $ "store " ++ show i ++ " " ++ show s ++ " " ++ show t ++ "\n"

instance {-# OVERLAPPING #-} Show a => Show (Tps Print a) where
  show (Leaf a) = "return " ++ show a
  show (Node (PrintWith f) ks  (Some (_,k))) = f (fmap show ks) ++ show k
  show (Node (Print s)     Nil (Some (_,k))) = s ++ show k
  show (Node (Done s)      Nil None)         = s

instance (Show a, Printable f) => Show (Tps f a) where show = show . pCmdT
instance {-# OVERLAPPING #-} (Show a, Printable f) => Show (Tps (Print :+: f) a) where show = show . morge . pCmdT' . swop
instance {-# OVERLAPPING #-} Show a => Show (Tps (Print :+: VoidCmd) a) where show = show . dropVoid
instance {-# OVERLAPPING #-} (Show (Tps (Print :+: cmd) a), Printable f) => Show (Tps (          f :+: cmd) a) where show = show . pCmdT'
instance {-# OVERLAPPING #-} (Show (Tps (Print :+: cmd) a), Printable f) => Show (Tps (Print :+: f :+: cmd) a) where show = show . merge . pCmdT' . swap

e :: Tps (Fix :+: Base) a
e = 
    Node (L (Fix (("f", ["x"]) ::: Nil)))
    ((Node (R (Add (VAR "x") (INT 1))) Nil (Some ("x", Leaf (VAR "x")))) ::: Nil) (Some ("",
    Node (R (App (VAR "f") [INT 41])) Nil None))

e' :: Tps (Malloc :+: Fix :+: Base :+: VoidCmd) Val
e' = do
  fix_ (("f",["x"],do
        add_ (VAR "x") (INT 1) "n"
        done (VAR "n"))
        ::: Nil)
  malloc_ 0 "x"
  app (VAR "f") [INT 41]
