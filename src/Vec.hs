{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Vec where

data Nat = Z | S Nat

infixr 9 :::
data Vec n a where
  Nil   :: Vec 'Z a
  (:::) :: a -> Vec n a -> Vec ('S n) a

instance Functor (Vec n) where
  fmap _ Nil = Nil
  fmap f (x ::: xs) = f x ::: fmap f xs

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

instance Foldable (Vec n) where
  foldMap _ Nil = mempty
  foldMap f (x ::: xs) = mappend (f x) (foldMap f xs)


  foldr _ b Nil = b
  foldr f b (x ::: xs) = f x (foldr f b xs)

  foldl _ b Nil = b
  foldl f b (x ::: xs) = foldl f (f b x) xs 

instance Traversable (Vec n) where
  traverse _ Nil = pure Nil
  traverse f (x ::: xs) = (:::) <$> f x <*> traverse f xs

instance Show a => Show (Vec n a) where
  show Nil = "Nil"
  show (x ::: xs) = show x ++ " : " ++ show xs

