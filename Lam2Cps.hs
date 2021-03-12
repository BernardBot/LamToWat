module Lam2Cps where

import Lam hiding (INT,M)
import Cps hiding (Int,M)

import Control.Monad.Cont
import Control.Monad.State

lam2cps :: Lam -> Cps
lam2cps =
  fst .
  flip runState 0 .
  flip runContT (return . DONE) .
  l2c

type M = ContT Cps (State Int)

l2c :: Lam -> M Val
l2c (Var x) = return (VAR x)
l2c (Int i) = return (INT i)
l2c (Lam x e) = ContT (\ c -> do
  f <- fresh "f"
  k <- fresh "k"
  cf <- runContT (l2c e) (\ z ->
          return (APP (VAR k) [z]))
  c' <- c (LABEL f)
  return (FIX [(f,[x,k],cf)] c'))
l2c (App e1 e2) = do
  v1 <- l2c e1
  v2 <- l2c e2
  ContT (\ c -> do
    r <- fresh "r"
    x <- fresh "x"
    c' <- c (VAR x)
    return (FIX [(r,[x],c')] (APP v1 [v2, LABEL r])))
l2c (Add e1 e2) = do
  v1 <- l2c e1
  v2 <- l2c e2
  ContT (\ c -> do
    x <- fresh "x"
    c' <- c (VAR x)
    return (ADD v1 v2 x c'))

fresh :: String -> State Int String
fresh s = do
  i <- get
  put (i+1)
  return $ "_" ++ s ++ show i

-- lam2cps :: Lam -> Cps
-- lam2cps =
--   fst .
--   flip runState 0 .
--   flip l2c (return . DONE)

-- type M = State Int

-- l2c :: Lam -> (Val -> M Cps) -> M Cps
-- l2c (Var x) c = c (VAR x)
-- l2c (Int i) c = c (INT i)
-- l2c (Lam x e) c = do
--   f <- fresh "f"
--   k <- fresh "k"
--   c' <- c (LABEL f)
--   cf <- l2c e (\ z ->
--          return $ APP (VAR k) [z])
--   return $ FIX [(f,[x,k],cf)] c'
-- l2c (App f e) c = do
--   r <- fresh "r"
--   x <- fresh "x"
--   c' <- c (VAR x)
--   cf <- l2c f (\ f' ->
--          l2c e (\ e' ->
--            return $ APP f' [e', LABEL r]))
--   return $ FIX [(r,[x],c')] cf
-- l2c (Add a b) c = do
--   x <- fresh "x"
--   c' <- c (VAR x)
--   l2c a (\ a' ->
--    l2c b (\ b' ->
--     return $ ADD a' b' x c'))

-- fresh s = do
--   i <- get
--   put (i+1)
--   return $ "_" ++ s ++ show i
