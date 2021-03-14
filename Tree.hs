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
import Data.List

data Tree sig a where
  Leaf :: a -> Tree sig a
  Node :: sig
       -> [Tree sig Val]
       -> Maybe (Val -> Tree sig a)
       -> Tree sig a

instance Monad (Tree sig) where
  Leaf a        >>= f = f a
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

--------------
-- Commands --
--------------

data Base
  = APP' Val [Val]
  | ADD' Val Val Val

data Fresh
  = FRESHLABEL' String
  | FRESHVAR' String

data Record
  = RECORD' [Val] Val
  | SELECT' Int Val Val

data Fun = FUN' Val [Val]

data Block
  = BLOCK'
  | SETK' String Val
  | GETK' String

data Malloc
  = MALLOC' Int Val
  | LOAD' Int Val Val
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

instance {-# OVERLAPS #-} a :<: a :+: b where
  inj = L
  prj (L a) = Just a
  prj _ = Nothing

instance a :<: c => a :<: b :+: c where
  inj = R . inj
  prj (R bc) = prj bc
  prj _ = Nothing

project :: sub :<: sup => Tree sup a -> Maybe (sub, [Tree sup Val], Maybe (Val -> Tree sup a))
project (Node cmd ks k) | Just cmd' <- prj cmd = Just (cmd',ks,k)
project _ = Nothing

--------------------------------------
-- Smart Constructors / Destructors --
--------------------------------------

app v vs = liftF (inj (APP' v vs)) []
pattern APP v vs <- (project -> Just (APP' v vs, [], Nothing))
add v1 v2 x = liftT (inj (ADD' v1 v2 x)) []
pattern ADD v1 v2 x k <- (project -> Just (ADD' v1 v2 x, [], Just k))

freshlabel x = liftT (inj (FRESHLABEL' x)) []
pattern FRESHLABEL x k <- (project -> Just (FRESHLABEL' x, [], Just k))
freshvar x = liftT (inj (FRESHVAR' x)) []
pattern FRESHVAR x k <- (project -> Just (FRESHVAR' x, [], Just k))

record vs x = liftT (inj (RECORD' vs x)) []
pattern RECORD vs x k <- (project -> Just (RECORD' vs x, [], Just k))
select i v x = liftT (inj (SELECT' i v x)) []
pattern SELECT i v x k <- (project -> Just (SELECT' i v x, [], Just k))

fun f as b = liftT (inj (FUN' f as)) [b]
pattern FUN f as b k <- (project -> Just (FUN' f as, [b], Just k))

block b = liftT (inj BLOCK') [b]
pattern BLOCK b k <- (project -> Just (BLOCK', [b], Just k))
setk x v = liftT (inj (SETK' x v)) []
pattern SETK x v k <- (project -> Just (SETK' x v, [], Just k))
getk x = liftT (inj (GETK' x)) []
pattern GETK x k <- (project -> Just (GETK' x, [], Just k))

malloc i x = liftT (inj (MALLOC' i x)) []
pattern MALLOC i x k <- (project -> Just (MALLOC' i x, [], Just k))
load i v x = liftT (inj (LOAD' i v x)) []
pattern LOAD i v x k <- (project -> Just (LOAD' i v x, [], Just k))
store i s t = liftT (inj (STORE' i s t)) []
pattern STORE i s t k <- (project -> Just (STORE' i s t, [], Just k))

-----------
-- Print --
-----------

tab = "  "
indent = unlines . map (tab++) . lines
assign x y = x ++ " = " ++ y ++ "\n"
args ss = "(" ++ intercalate "," ss ++ ")"

instance Show Val where
  show (VAR x) = x
  show (LABEL x) = x
  show (INT i) = show i

instance Show (Tree (Record :+: Fun :+: Base) Val) where
  show (Leaf v)         = show v
  show (ADD v1 v2 x k)  = assign (show x) (show v1 ++ " + " ++ show v2) ++ show (k x)
  show (APP v vs)       = show v ++ args (map show vs)
  show (FUN f as b k)   = "def " ++ show f ++ args (map show as) ++ ":\n" ++ indent (show b) ++ show (k f)
  show (RECORD vs x k)  = assign (show x) (show vs                    ) ++ show (k x)
  show (SELECT n v x k) = assign (show x) (show v ++ " !! " ++ show n ) ++ show (k x)

