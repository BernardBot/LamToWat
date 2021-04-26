{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CTree.Option where

data Option :: Bool -> * -> * where
  Some :: a -> Option True a
  None ::      Option False a

deriving instance Show a => Show (Option b a)
deriving instance Functor (Option b)
