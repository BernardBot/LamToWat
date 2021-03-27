{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Tree where

import Control.Monad
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
  Plus  :: Val -> Val ->               Cmd Z     True  Void  Void Val
  App   :: Val -> [Val] ->             Cmd Z     False Void  Void Val
  Fix   :: Vec n (String, [String]) -> Cmd n     True  ()    Val  ()
  GetK  :: String ->                   Cmd Z     True  Val   Val  Val
  SetK  :: String -> Val ->            Cmd Z     True  Val   Val  ()
  Block ::                             Cmd (S Z) True  ()    Val  Val
  Fresh :: String ->                   Cmd Z     True  Void  Void String

----------------------------
--- boilerplate liftings ---
----------------------------

plus :: Val -> Val -> Tree Cmd Val
plus v1 v2 = liftT (Plus v1 v2) Nil

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

instance Show Val where
  show (INT i) = show i
  show (VAR x) = x
  show (LABEL x) = x

instance Show (Cmd n b p r q) where
  show (Plus v1 v2) = "(Plus " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (App v vs)   = "(App " ++ show v ++ " " ++ show vs ++ ")"
  show (Fix fxs)    = "(Fix " ++ show fxs ++ ")"
  show (SetK x v)   = "(SetK " ++ x ++ " " ++ show v ++ ")"
  show (GetK x)     = "(GetK " ++ x ++ ")"
  show Block        = "(Block)"
  show (Fresh x)    = "(Fresh " ++ x ++ ")"

---------------------------------------
--- show instance for command trees ---
---------------------------------------

pprint :: Show a => Tree Cmd a -> Int -> String
pprint (Leaf x) _ = show x
pprint (Node op@(Plus _ _) Nil (Some k)) i =
  let x = "x" ++ show i
  in show op ++ " (λ " ++ x ++ " → " ++ pprint (k (VAR x)) (i + 1) ++ ")"
pprint (Node op@(App _ _) Nil None) _ =
  show op
pprint (Node op@(Fix _) ks (Some k)) i =
  show op ++ " [" ++
  foldr (\ k1 s -> (pprint (k1 ()) (i + 1)) ++ "; " ++ s) "" ks ++ "] " ++
  pprint (k ()) (i + 1)
pprint (Node op@(GetK _) Nil (Some k)) i =
  let x = "x" ++ show i
  in show op ++ " (λ " ++ x ++ " → " ++
     pprint (k (VAR x)) (i + 1) ++ ")"
pprint (Node op@(SetK _ _) Nil (Some k)) i =
  let x = "x" ++ show i
  in show op ++ " (λ " ++ x ++ " → " ++
     pprint (k ()) (i + 1) ++ ")"
pprint (Node op@Block (k0 ::: Nil) (Some k)) i =
  let x        = "x" ++ show i
  in show op ++ " [" ++
     pprint (k0 ()) (i + 1) ++ "] (λ " ++ x ++ " → " ++ 
     pprint (k (VAR x)) (i + 1) ++ ")"
pprint (Node op@(Fresh _) Nil (Some k)) i =
  let x = "x" ++ show i
  in show op ++ " (λ " ++ x ++ " → " ++
     pprint (k x) (i + 1) ++ ")"

instance Show (Tree Cmd Val) where
  show t = pprint t 0
