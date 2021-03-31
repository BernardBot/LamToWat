{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Tree where

import Control.Monad
import Control.Monad.State
import Data.Void

import Vec
import Option
import Val

data Tree sig a where
  Leaf :: a -> Tree sig a
  Node :: sig n b p r q ->
          Vec n (p -> Tree sig r) ->
          Option b (q -> Tree sig a) ->
          Tree sig a

instance Monad (Tree sig) where
  Leaf x       >>= g = g x
  Node op cs k >>= g = Node op cs (fmap (\ k x -> k x >>= g) k)

instance Functor (Tree sig) where
  fmap = liftM

instance Applicative (Tree sig) where
  pure = Leaf
  (<*>) = ap

liftT op ps = Node op ps (Some Leaf)
liftF op ps = Node op ps None

------------------
-- CPS Commands --
------------------

data Cmd :: Nat -> Bool -> * -> * -> * -> * where
  Add   :: Val -> Val ->               Cmd Z     True  Void  Void Val
  App   :: Val -> [Val] ->             Cmd Z     False Void  Void Val
  Fix   :: Vec n (String, [String]) -> Cmd n     True  ()    Val  ()
  GetK  :: String ->                   Cmd Z     True  Val   Val  Val
  SetK  :: String -> Val ->            Cmd Z     True  Val   Val  ()
  Block ::                             Cmd (S Z) True  ()    Val  Val
  Fresh :: String ->                   Cmd Z     True  Void  Void String

----------------------------
--- boilerplate liftings ---
----------------------------

add :: Val -> Val -> Tree Cmd Val
add v1 v2 = liftT (Add v1 v2) Nil

app :: Val -> [Val] -> Tree Cmd Val
app v vs = liftF (App v vs) Nil

data VPair a b n = VPair { getVPair :: (Vec n a, Vec n b) }

fix :: Vec n (String, [String], Tree Cmd Val) -> Tree Cmd ()
fix fs =
  let VPair (fxs, bs) =
        ifoldV (VPair (Nil, Nil))
               (\ (f, xs, b) (VPair (fxs, bs)) ->
                  VPair ((f, xs) ::: fxs, b ::: bs))
               fs
  in liftT (Fix fxs) (mapV const bs)

setk :: String -> Val -> Tree Cmd ()
setk x v = liftT (SetK x v) Nil

getk :: String -> Tree Cmd Val
getk x = liftT (GetK x) Nil

block :: Tree Cmd Val -> Tree Cmd Val
block b = liftT Block (const b ::: Nil)

fresh :: String -> Tree Cmd String
fresh v = liftT (Fresh v) Nil

---------------------
--- show instance ---
---------------------

instance Show (Cmd n b p r q) where
  show (Add v1 v2) = "(Add " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (App v vs)  = "(App " ++ show v ++ " " ++ show vs ++ ")"
  show (Fix fxs)   = "(Fix " ++ show fxs ++ ")"
  show (SetK x v)  = "(SetK " ++ x ++ " " ++ show v ++ ")"
  show (GetK x)    = "(GetK " ++ x ++ ")"
  show Block       = "(Block)"
  show (Fresh x)   = "(Fresh " ++ x ++ ")"

---------------------------------------
--- show instance for command trees ---
---------------------------------------

instance Show (Tree Cmd Val) where
  show = fst . flip runState 0 . go
    where go :: Tree Cmd Val -> State Int String
          go (Leaf v) = return $ show v
          go (Node op@(Add _ _) Nil (Some k)) = do
            x <- fresh "x"
            k' <- go (k (VAR x))
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"
          go (Node op@(App _ _) Nil None) = return $ show op
          go (Node op@(Fix _) ks (Some k)) = do
            ks' <- mapM (go . ($())) ks
            k' <- go (k ())
            return $ show op ++ " [" ++
              foldr (\ k s -> k ++ "; " ++ s) "" ks' ++ "] " ++
              k'
          go (Node op@(GetK _) Nil (Some k)) = do
            x <- fresh "x"
            k' <- go (k (VAR x))
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"
          go (Node op@(SetK _ _) Nil (Some k)) = do
            x <- fresh "x"
            k' <- go (k ())
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"
          go (Node op@Block (k0 ::: Nil) (Some k)) = do
            x <- fresh "x"
            k0' <- go (k0 ())
            k' <- go (k (VAR x))
            return $ show op ++ " [" ++
              k0' ++ "] (λ " ++ x ++ " → "
              ++ k' ++ ")"
          go (Node op@(Fresh y) Nil (Some k)) = do
            x <- fresh y
            k' <- go (k x)
            return $ show op ++ " (λ " ++ x ++ " → " ++ k' ++ ")"

          fresh :: String -> State Int String
          fresh s = do
            i <- get
            put (i+1)
            return $ "_" ++ s ++ show i
