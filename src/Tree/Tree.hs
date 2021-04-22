{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tree where

import Control.Monad
import Control.Monad.State
import Data.Void

import Types hiding (fix,Fix)

import Vec
import Option

data Tree sig a where
  Leaf :: a -> Tree sig a
  Node :: sig n b p r q ->
          Vec n (p -> Tree sig r) ->
          Option b (q -> Tree sig a) ->
          Tree sig a

instance Monad (Tree sig) where
  Leaf x       >>= g = g x
  Node op cs k >>= g = Node op cs (fmap (\ k x -> k x >>= g) k)

instance Functor (Tree sig) where
  fmap = liftM

instance Applicative (Tree sig) where
  pure = Leaf
  (<*>) = ap

liftT op ps = Node op ps (Some Leaf)
liftF op ps = Node op ps None

------------------
-- CPS Commands --
------------------

type Sig = Nat -> Bool -> * -> * -> * -> *

data Base :: Sig where
  Add   :: Val -> Val ->               Base Z     True  Void  Void Val
  App   :: Val -> [Val] ->             Base Z     False Void  Void Val

data Fix :: Sig where
  Fix   :: Vec n (String, [String]) -> Fix  n     True  ()    Val  ()

data Comp :: Sig where
  GetK  :: String ->                   Comp Z     True  Val   Val  Val
  SetK  :: String -> Val ->            Comp Z     True  Val   Val  ()
  Block ::                             Comp (S Z) True  ()    Val  Val
  Fresh :: String ->                   Comp Z     True  Void  Void String

---------------
-- injection --
---------------

data (:+:) :: Sig -> Sig -> Sig where
  L :: sigl n b p r q -> (sigl :+: sigr) n b p r q
  R :: sigr n b p r q -> (sigl :+: sigr) n b p r q
infixr 7 :+:

class (sub :: Sig) :<: (sup :: Sig) where
  inj :: sub n b p r q -> sup n b p r q

instance f :<: f where
  inj = id

instance {-# OVERLAPPING #-} f :<: (f :+: g) where
  inj = L

instance f :<: g => f :<: (h :+: g) where
  inj = R . inj

----------------------------
--- boilerplate liftings ---
----------------------------

add v1 v2 = liftT (inj (Add v1 v2)) Nil
app v vs = liftF (inj (App v vs)) Nil

data VPair a b n = VPair { getVPair :: (Vec n a, Vec n b) }
fix fs =
  let VPair (fxs, bs) =
        ifoldV (VPair (Nil, Nil))
               (\ (f, xs, b) (VPair (fxs, bs)) ->
                  VPair ((f, xs) ::: fxs, b ::: bs))
               fs
  in liftT (inj (Fix fxs)) (mapV const bs)

setk x v = liftT (inj (SetK x v)) Nil
getk x = liftT (inj (GetK x)) Nil
block b = liftT (inj Block) (const b ::: Nil)
fresh v = liftT (inj (Fresh v)) Nil

---------------------
--- show instance ---
---------------------

instance Show (Base n b p r q) where
  show (Add v1 v2) = "(Add " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (App v vs)  = "(App " ++ show v ++ " " ++ show vs ++ ")"

instance Show (Fix n b p r q) where
  show (Fix fxs)   = "(Fix " ++ show fxs ++ ")"

instance Show (Comp n b p r q) where
  show (SetK x v)  = "(SetK " ++ x ++ " " ++ show v ++ ")"
  show (GetK x)    = "(GetK " ++ x ++ ")"
  show Block       = "(Block)"
  show (Fresh x)   = "(Fresh " ++ x ++ ")"

---------------------------------------
--- show instance for command trees ---
---------------------------------------

type Cmd = Comp :+: Fix :+: Base

instance Show a => Show (Tree Cmd a) where
  show = fst . flip runState 0 . go
    where go :: Show a => Tree Cmd a -> State Int String
          go (Leaf v) = return $ show v
          go (Node (R (R op@(Add _ _))) Nil (Some k)) = do
            x <- fresh "x"
            k' <- go (k (VAR x))
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"
          go (Node (R (R op@(App _ _))) Nil None) = return $ show op
          go (Node (R (L op@(Fix _))) ks (Some k)) = do
            ks' <- mapM (go . ($())) ks
            k' <- go (k ())
            return $ show op ++ " [" ++
              foldr (\ k s -> k ++ "; " ++ s) "" ks' ++ "] " ++
              k'
          go (Node (L op@(GetK _)) Nil (Some k)) = do
            x <- fresh "x"
            k' <- go (k (VAR x))
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"
          go (Node (L op@(SetK _ _)) Nil (Some k)) = do
            x <- fresh "x"
            k' <- go (k ())
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"
          go (Node (L op@Block) (k0 ::: Nil) (Some k)) = do
            x <- fresh "x"
            k0' <- go (k0 ())
            k' <- go (k (VAR x))
            return $ show op ++ " [" ++
              k0' ++ "] (λ " ++ x ++ " → "
              ++ k' ++ ")"
          go (Node (L op@(Fresh y)) Nil (Some k)) = do
            x <- fresh y
            k' <- go (k x)
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"

          fresh :: String -> State Int String
          fresh s = do
            i <- get
            put (i+1)
            return $ "_" ++ s ++ show i

--------------
-- Examples --
--------------

e :: Tree Cmd Val
e = Node (R (L (Fix (("f", ["x"]) ::: Nil)))) ((\ () ->
      Node (R (R (Add (VAR "x") (INT 1)))) Nil (Some (\ n -> Leaf n))) ::: Nil) (Some (\ () ->
    Node (R (R (App (VAR "f") [INT 41]))) Nil None))
