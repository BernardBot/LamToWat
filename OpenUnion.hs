{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpenUnion where

----------------------------
-- Injection / Projection --
----------------------------

infixr 7 :+:
data (f :+: g) a = L (f a) | R (g a) deriving (Show, Functor)

infixr 6 :<:
class sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance f :<: f where
  inj = id
  prj = Just

instance f :<: f :+: g where
  inj = L
  prj (L a) = Just a
  prj _ = Nothing

instance {-# OVERLAPPABLE #-} f :<: h => f :<: g :+: h where
  inj = R . inj
  prj (R b) = prj b
  prj _ = Nothing

--------------
-- FixPoint --
--------------

data Fix f where
  Fix :: f (Fix f) -> Fix f

inject :: g :<: f => g (Fix f) -> Fix f
inject = Fix . inj

project :: f :<: g => Fix g -> Maybe (f (Fix g))
project (Fix f) = prj f
