{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CTree.Commands where

import Data.Data

import Control.Monad.Reader (ask,local)
import Data.Void

import Interpreter
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

instance Interpretable (Base n b p r q) where
  interp (Add v1 v2) = do
    Int i <- interp v1
    Int j <- interp v2
    return $ Int $ i + j
  interp (App v vs) = do
    Fun f <- interp v
    ds <- mapM interp vs
    f ds

instance Interpretable (Fix n b p r q) where
  interp (Fix fxs) = do
    nv <- ask
    return $ Fun $ \ (Fun k:ks) -> do -- hacky
      let fs = zipWith (\ (f,as) (Fun k) -> (f,as,k [])) (toList fxs) ks
          nv' = funs ++ nv
          funs = map (\ (f,as,b) -> (f,clos nv' as b)) fs
      local (const nv') (k [])

-- instance Interpretable (Comp n b p r q) where

instance Interpretable (Record n b p r q) where
  interp (CTree.Commands.Record vs) = do
    ds <- mapM interp vs
    return $ Interpreter.Record ds
  interp (Select i v) = do
    Interpreter.Record ds <- interp v
    return $ ds !! i

instance Interpretable (Malloc n b p r q) where
  interp (Malloc i) = do
    p <- malloc i
    return $ Int p
  interp (Store i s t) = do
    Int j <- interp s
    d     <- interp t
    store (i+j) d
    return $ Int 0
  interp (Load i v) = do
    Int j <- interp v
    load (i+j)

instance Interpretable (Empty n b p r q) where
  interp empty = case empty of
