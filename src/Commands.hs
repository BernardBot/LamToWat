{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Commands where

import Data.Void

import Types hiding (Fix,Record)

import Option
import Vec

data Base :: Sig where
  Add :: Val -> Val -> Base Z True Void Void Val
  App :: Val -> [Val] -> Base Z False Void Void Val

data Fix :: Sig where
  Fix :: Vec n (Var,[Var]) -> Fix n True () Val ()

data Comp :: Sig where
  GetK :: Var -> Comp Z True Val Val Val
  SetK :: Var -> Val -> Comp Z True Val Val ()
  Block :: Comp (S Z) True () Val Val
  Fresh :: Var -> Comp Z True Void Void Var

data Record :: Sig where
  Record :: [Val] -> Record Z True Void Void Val
  Select :: Int -> Val -> Record Z True Void Void Val

data Malloc :: Sig where
  Malloc :: Int -> Malloc Z True Void Void Val
  Load :: Int -> Val -> Malloc Z True Void Void Val
  Store :: Int -> Val -> Val -> Malloc Z True Void Void ()

data Empty :: Sig where
  
deriving instance Show (Base n b p r q)
deriving instance Show (Fix n b p r q)
deriving instance Show (Comp n b p r q)
deriving instance Show (Record n b p r q)
deriving instance Show (Malloc n b p r q)
deriving instance Show (Empty n b p r q)

instance ShowSig Base where showSig = show
instance ShowSig Fix where showSig = show
instance ShowSig Comp where showSig = show
instance ShowSig Record where showSig = show
instance ShowSig Malloc where showSig = show
instance ShowSig Empty where showSig = show

instance PPrintable (Base n b p r q) where
  pprint (App v vs) = pprint v ++ args (map pprint vs)
  pprint (Add v1 v2) = pprint v1 ++ " + " ++ pprint v2

instance PPrintable (Record n b p r q) where
  pprint (Record vs) = pprint vs
  pprint (Select i v) = pprint v ++ "[" ++ show i ++ "]"

instance PPrintable (Malloc n b p r q) where
  pprint (Malloc i) = "malloc " ++ show i
  pprint (Load i v) = "load " ++ show i ++ " " ++ pprint v
  pprint (Store i s t) = "store " ++ show i ++ " " ++ pprint s ++ " " ++ pprint t
