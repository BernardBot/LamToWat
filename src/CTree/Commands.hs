{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CTree.Commands where

import Data.Void

import Val
import Types hiding (Fix,Record)

import CTree.Option
import CTree.Vec

data Base :: Sig where
  Add    :: Val -> Val ->        Base Z     True  Void Void Val
  App    :: Val -> [Val] ->      Base Z     False Void Void Val

data Fix :: Sig where
  Fix    :: Vec n (Var,[Var]) -> Fix n      True  ()   Val  ()

data Comp :: Sig where
  GetK   :: Var ->               Comp Z     True  Val  Val  Val
  SetK   :: Var -> Val ->        Comp Z     True  Val  Val  ()
  Block  ::                      Comp (S Z) True  ()   Val  Val
  Fresh  :: Var ->               Comp Z     True  Void Void Var

data Record :: Sig where
  Record :: [Val] ->             Record Z   True  Void Void Val
  Select :: Int -> Val ->        Record Z   True  Void Void Val

data Malloc :: Sig where
  Malloc :: Int ->               Malloc Z   True  Void Void Val
  Load   :: Int -> Val ->        Malloc Z   True  Void Void Val
  Store  :: Int -> Val -> Val -> Malloc Z   True  Void Void ()

data Empty :: Sig where

deriving instance Show (Base   n b p r q)
deriving instance Show (Fix    n b p r q)
deriving instance Show (Comp   n b p r q)
deriving instance Show (Record n b p r q)
deriving instance Show (Malloc n b p r q)
deriving instance Show (Empty  n b p r q)
