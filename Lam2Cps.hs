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

type N = StateT Int (Cont Cps)
type M = ContT Cps (State Int)

bar = 
  flip runCont (DONE . fst) .
  flip runStateT 0 .
  foo

foo :: Lam -> N Val
foo (Var x) = return (VAR x)
foo (Lam x e) = do
  f <- fresh "x"
  k <- fresh "k"
  a <- lift $ mapCont (\ v -> FIX [(f,[x,k],v)] (DONE (LABEL "f"))) (flip runStateT 0 $ foo e)
  put (snd a)
  foo (Var "kaasman")

l2c :: Lam -> M Val
l2c (Var x) = return (VAR x)
l2c (Int i) = return (INT i)
l2c (Lam x e) = ContT $ \ c -> do
  f <- fresh "f"
  k <- fresh "k"
  cf <- l2c' e
  c' <- c (LABEL f)
  return (FIX [(f,[x,k],cf)] c')
l2c (App e1 e2) = do
  v1 <- l2c e1
  v2 <- l2c e2
  undefined
--  kont $ \ r -> APP v1 [v2,r]
l2c (Add e1 e2) = do
  r <- fresh "r"
  x <- fresh "x"
  undefined

l2c' e = runContT (l2c e) (return . DONE)

-- so promising...
add v1 v2 = ContT $ \ c -> do
  x <- fresh "x"
  c' <- c (VAR x)
  return $ ADD v1 v2 x c'

kont k = do
  r <- fresh "r"
  x <- fresh "x"
  ContT $ \ c -> do
    c' <- c (VAR x)
    k' <- k (LABEL r)
    return $ FIX [(r,[x],c')] k'


-- fun fs bk = ContT $ \ c -> do
--   cf <- runContT (l2c e) (return . bk)
--   c' <- c (LABEL f)
--   return $ FIX [(f,as,cf)] c'

fresh s = do
  i <- get
  put (i+1)
  return $ "_" ++ s ++ show i
