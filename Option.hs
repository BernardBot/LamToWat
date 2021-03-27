{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Option where

data Option :: Bool -> * -> * where
  Some :: a -> Option True a
  None ::      Option False a

instance Functor (Option b) where
  fmap f (Some x) = Some (f x)
  fmap f None     = None
