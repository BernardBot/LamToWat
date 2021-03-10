{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Tree where

import Control.Monad
import Data.Maybe

data Tree sig a where
  Leaf :: a -> Tree sig a
  Node :: sig
       -> [[Val] -> Tree sig Val]
       -> Maybe (Val -> Tree sig a)
       -> Tree sig a

instance Monad (Tree sig) where
  Leaf a >>= f = f a
  Node sig ks k >>= f = Node sig ks (fmap (\ k x -> k x >>= f) k)

instance Applicative (Tree sig) where
  pure = Leaf
  (<*>) = ap

instance Functor (Tree sig) where
  fmap = liftM

liftT f ks = Node f ks (Just Leaf)
liftF f ks = Node f ks Nothing

data Val
  = INT Int
  | VAR String
  | LABEL String
  deriving Eq

val2strl :: Val -> [String]
val2strl (VAR x) = [x]
val2strl (LABEL x) = [x]
val2strl (INT i) = []

--------------
-- Commands --
--------------

data Base
  = APP' Val [Val]
  | ADD' Val Val
  | SET' String Val

data Record
  = RECORD' [Val]
  | SELECT' Int Val

data Fun = FUN' Int  

data Block
  = BLOCK'
  | SETK' String Val
  | GETK' String

data Malloc
  = MALLOC' Int
  | LOAD' Int Val
  | STORE' Int Val Val

---------------
-- Injection --
---------------

infixr 7 :+:
data a :+: b = L a | R b

infixr 6 :<:
class sub :<: sup where
  inj :: sub -> sup
  prj :: sup -> Maybe sub

instance a :<: a where
  inj = id
  prj = Just

instance a :<: a :+: b where
  inj = L
  prj (L a) = Just a
  prj _ = Nothing

instance a :<: c => a :<: b :+: c where
  inj = R . inj
  prj (R bc) = prj bc
  prj _ = Nothing

project :: sub :<: sup => Tree sup a -> Maybe (sub, [[Val] -> Tree sup Val], Maybe (Val -> Tree sup a))
project (Node cmd ks k) | Just cmd' <- prj cmd = Just (cmd',ks,k)
project _ = Nothing

--------------------------------------
-- Smart Constructors / Destructors --
--------------------------------------

app v vs = liftF (inj (APP' v vs)) []
pattern APP v vs <- (project -> Just (APP' v vs, [], Nothing))
add v1 v2 = liftT (inj (ADD' v1 v2)) []
pattern ADD v1 v2 k <- (project -> Just (ADD' v1 v2, [], Just k))
set x v = liftT (inj (SET' x v)) []
pattern SET x v k <- (project -> Just (SET' x v, [], Just k))

record vs = liftT (inj (RECORD' vs)) []
pattern RECORD vs k <- (project -> Just (RECORD' vs, [], Just k))
select i v = liftT (inj (SELECT' i v)) []
pattern SELECT i v k <- (project -> Just (SELECT' i v, [], Just k))

fun i f = liftT (inj (FUN' i)) [f]
pattern FUN i b k <- (project -> Just (FUN' i, [b], Just k))

block b = liftT (inj BLOCK') [const b]
pattern BLOCK b k <- (project -> Just (BLOCK', [b], Just k))
setk x v = liftT (inj (SETK' x v)) []
pattern SETK x v k <- (project -> Just (SETK' x v, [], Just k))
getk x = liftT (inj (GETK' x)) []
pattern GETK x k <- (project -> Just (GETK' x, [], Just k))

malloc i = liftT (inj (MALLOC' i)) []
pattern MALLOC i k <- (project -> Just (MALLOC' i, [], Just k))
load i v = liftT (inj (LOAD' i v)) []
pattern LOAD i v k <- (project -> Just (LOAD' i v, [], Just k))
store i s t = liftT (inj (STORE' i s t)) []
pattern STORE i s t k <- (project -> Just (STORE' i s t, [], Just k))
