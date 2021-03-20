{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Tree hiding (add)

import Control.Monad
import Control.Monad.State

class Cell d where
  fetch :: d
  set :: d -> d

class Addition d where
  add :: d -> d -> d
  int :: Int -> d

------------------------
-- Monad Transformers --
------------------------

instance (Monad m, MonadState s m) => MonadFail m where
  fail = error

instance {-# OVERLAPPABLE #-} MonadState Int m => Cell (m Int) where
  fetch = get
  set d = do
    d' <- d
    put d'
    return d'

instance {-# OVERLAPPABLE #-} MonadState Int m => Addition (m Int) where
  add a b = do
    i <- a
    j <- b
    return (i + j)
  int = return

interpM :: State Int c -> c
interpM = fst . flip runState 0

eM0 :: State Int Int
eM0 = add (set (int 10)) fetch

-----------------------
-- Algebraic Effects --
-----------------------

data Eff' cmd d where
  Pure :: d -> Eff' cmd d
  Impure :: cmd -> (Int -> Eff' cmd d) -> Eff' cmd d

data Cmd = Get | Put Int

instance Functor (Eff' cmd) where
  fmap = liftM

instance Applicative (Eff' cmd) where
  pure = Pure
  (<*>) = ap

instance Monad (Eff' cmd) where
  Pure d       >>= f = f d
  Impure cmd g >>= f = Impure cmd (\ x -> g x >>= f)

instance MonadFail (Eff' cmd) where
  fail = error

liftE x = Impure x Pure

get' :: Cmd :<: cmd => Eff' cmd Int
get' = liftE (inj Get)

put' :: Cmd :<: cmd => Int -> Eff' cmd Int
put' = liftE . inj . Put

instance Cmd :<: cmd => Cell (Eff' cmd Int) where
  fetch = get'
  set d = do
    d' <- d
    put' d'
    
instance Cmd :<: cmd => Addition (Eff' cmd Int) where
  add a b = do
    i <- a
    j <- b
    return (i + j)
  int = return

hCmd :: Cmd :<: cmd => Eff' cmd a -> Int -> Eff' cmd (a, Int)
hCmd (Pure d)           s = Pure (d,s)
hCmd (Impure cmd k) s = case prj cmd of
  Just Get     -> hCmd (k s) s
  Just (Put s) -> hCmd (k s) s
  _            -> Impure cmd (\ d -> hCmd (k d) s)

interpA :: Cmd :<: cmd => Eff' cmd a -> a
interpA eff = case hCmd eff 0 of Pure (d,s) -> d

eA0 :: Eff' Cmd Int
eA0 = add (set (int 10)) fetch
