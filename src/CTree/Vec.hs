{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module CTree.Vec where

import Types

infixr 9 :::
data Vec n a where
  Nil   :: Vec 'Z a
  (:::) :: a -> Vec n a -> Vec ('S n) a

deriving instance Show a => Show (Vec n a)
deriving instance Functor (Vec n)
deriving instance Traversable (Vec n)
deriving instance Foldable (Vec n)

ifoldV :: b 'Z -> (forall n. a -> b n -> b ('S n)) -> Vec n a -> b n
ifoldV base _   Nil        = base
ifoldV base ind (a ::: as) = ind a (ifoldV base ind as)

mapV :: (a -> b) -> Vec n a -> Vec n b
mapV _ Nil        = Nil
mapV f (a ::: as) = f a ::: mapV f as

toList :: Vec n a -> [a]
toList Nil = []
toList (x ::: xs) = x : toList xs

zipWithV :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWithV _ Nil Nil = Nil
zipWithV f (a:::as) (b:::bs) = f a b ::: zipWithV f as bs
