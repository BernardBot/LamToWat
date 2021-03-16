{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Tree where

import Control.Monad
import Control.Monad.State

import Data.List

data Tree cmd a where
  Leaf :: a -> Tree cmd a
  Node :: cmd
       -> [Tree cmd Val]
       -> Maybe (Val -> Tree cmd a)
       -> Tree cmd a

instance Monad (Tree cmd) where
  Leaf a        >>= f = f a
  Node cmd ks k >>= f = Node cmd ks (fmap (\ k x -> k x >>= f) k)

instance Applicative (Tree cmd) where
  pure = Leaf
  (<*>) = ap

instance Functor (Tree cmd) where
  fmap = liftM

instance MonadFail (Tree cmd) where
  fail = error

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
  | ADD' Val Val String

data Record
  = RECORD' [Val] String
  | SELECT' Int Val String

data Fun = FUN' String [String]

data Fresh = FRESH' String

data Block
  = BLOCK'
  | SETK' String Val
  | GETK' String

data Malloc
  = MALLOC' Int String
  | LOAD' Int Val String
  | STORE' Int Val Val

----------------------------
-- Injection / Projection --
----------------------------

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

instance {-# OVERLAPPABLE #-} a :<: c => a :<: b :+: c where
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

fresh x = liftT (inj (FRESH' x)) []
pattern FRESH x k <- (project -> Just (FRESH' x, [], Just k))

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

------------------
-- Pretty Print --
------------------

-- how to do printing modularly?

instance Show Val where
  show (VAR x) = x
  show (LABEL x) = x
  show (INT i) = show i

tab = "  "
indent = unlines . map (tab++) . lines
assign x y = x ++ " = " ++ y ++ "\n"
args ss = "(" ++ intercalate "," ss ++ ")"

mkVar i s = intercalate "_" (map show s) ++ "__" ++ show i

-- use state monad...
vresh :: String -> State Int String
vresh s = do
  i <- get
  put (i+1)
  return $ "_" ++ s ++ show i

pprint (Leaf v) = return (show v)
pprint (ADD v1 v2 x k) = do
  x <- vresh "x"
  k' <- pprint (k (VAR x))
  return (assign x (show v1 ++ " + " ++ show v2) ++ k')
pprint (APP v vs) = return (show v ++ args (map show vs))
pprint (FUN f as b k) = do
  b' <- pprint b
  k' <- pprint (k (LABEL f))
  return ("def " ++ f ++ args as ++ ":\n" ++ indent b' ++ k')
pprint (FRESH x k) = do
  f <- vresh x
  k' <- pprint (k (VAR f))
  return (assign f ("fresh " ++ x) ++ k')
pprint (BLOCK b k) = do
  x <- vresh "b"
  b' <- pprint b
  k' <- pprint (k (VAR x))
  return ("block " ++ x ++ "\n{\n" ++ indent b' ++ "}\n" ++ k')
pprint (GETK x k) = do
  g <- vresh "g"
  k' <- pprint (k (VAR g))
  return (assign g ("getk " ++ x) ++ k')
pprint (SETK x v k) = do
  k' <- pprint (k (INT 0))
  return ("setk " ++ x ++ " " ++ show v ++ "\n" ++ k')

instance Show (Tree (Block :+: Fresh :+: Fun :+: Base) Val) where
  show = fst . flip runState 0 . pprint

pprint' (Leaf v) = return (show v)
pprint' (ADD v1 v2 x k) = do
  x <- vresh "x"
  k' <- pprint' (k (VAR x))
  return (assign x (show v1 ++ " + " ++ show v2) ++ k')
pprint' (APP v vs) = return (show v ++ args (map show vs))
pprint' (FUN f as b k) = do
  b' <- pprint' b
  k' <- pprint' (k (LABEL f))
  return ("def " ++ f ++ args as ++ ":\n" ++ indent b' ++ k')
pprint' (FRESH x k) = do
  f <- vresh x
  k' <- pprint' (k (VAR f))
  return (assign f ("fresh " ++ x) ++ k')

instance Show (Tree (Fresh :+: Fun :+: Base) Val) where
  show = fst . flip runState 0 . pprint'

instance Show (Tree (Fun :+: Base) Val) where
  show (Leaf v)         = show v
  show (ADD v1 v2 x k)  = assign x (show v1 ++ " + " ++ show v2) ++ show (k (VAR x))
  show (APP v vs)       = show v ++ args (map show vs)
  show (FUN f as b k)   = "def " ++ f ++ args as ++ ":\n" ++ indent (show b) ++ show (k (LABEL f))

instance Show (Tree (Record :+: Fun :+: Base) Val) where
  show (Leaf v)         = show v
  show (ADD v1 v2 x k)  = assign x (show v1 ++ " + " ++ show v2) ++ show (k (VAR x))
  show (APP v vs)       = show v ++ args (map show vs)
  show (FUN f as b k)   = "def " ++ f ++ args as ++ ":\n" ++ indent (show b) ++ show (k (LABEL f))
  show (RECORD vs x k)  = assign x (show vs                    ) ++ show (k (VAR x))
  show (SELECT n v x k) = assign x (show v ++ "[" ++ show n ++ "]") ++ show (k (VAR x))

instance Show (Tree (Malloc :+: Fun :+: Base) Val) where
  show (Leaf v)         = show v
  show (ADD v1 v2 x k)  = assign x (show v1 ++ " + " ++ show v2) ++ show (k (VAR x))
  show (APP v vs)       = show v ++ args (map show vs)
  show (FUN f as b k)   = "def " ++ f ++ args as ++ ":\n" ++ indent (show b) ++ show (k (LABEL f))
  show (MALLOC i x k)   = assign x ("malloc " ++ show i) ++ show (k (VAR x))
  show (LOAD i v x k)   = assign x ("load " ++ show i ++ " " ++ show v) ++ show (k (VAR x))
  show (STORE i s t k)  = "store " ++ show i ++ " " ++ show s ++ " " ++ show t ++ "\n" ++ show (k (INT 0))

instance Show (Tree (Malloc :+: Base) Val) where
  show (Leaf v)         = show v
  show (ADD v1 v2 x k)  = assign x (show v1 ++ " + " ++ show v2) ++ show (k (VAR x))
  show (APP v vs)       = show v ++ args (map show vs)
  show (MALLOC i x k)   = assign x ("malloc " ++ show i) ++ show (k (VAR x))
  show (LOAD i v x k)   = assign x ("load " ++ show i ++ " " ++ show v) ++ show (k (VAR x))
  show (STORE i s t k)  = "store " ++ show i ++ " " ++ show s ++ " " ++ show t ++ "\n" ++ show (k (INT 0))
