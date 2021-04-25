{-# LANGUAGE FlexibleInstances #-}

module Interpreter where

import Control.Monad.State
import Control.Monad.Reader

import Types

class Interpretable a where
  interp :: a -> IDom

-- Environment
type Env = [(Var,Dom)]
-- Heap
type Pointer = Int
type Heap = (Pointer,[Dom])
-- Errors
type Error = String
instance MonadFail (Either Error) where
  fail = Left
-- Monad Transformer
type InterpM = StateT Heap (ReaderT Env (Either Error))
type EDom = Either Error Dom
type IDom = InterpM Dom
-- Domain
data Dom
  = Record [Dom]
  | Int Int
  | Fun ([Dom] -> IDom)

instance Show Dom where
  show (Record ds) = show ds
  show (Int i)     = show i
  show (Fun _)     = "fun"

instance Eq Dom where
  Int i     == Int j     = i == j
  Record xs == Record ys = xs == ys
  _         == _         = False

---------------------
-- Run interpreter --
---------------------

runInterp :: Env -> Heap -> IDom -> EDom
runInterp env heap =
  fmap fst .
  flip runReaderT env .
  flip runStateT heap

run :: Interpretable a => Env -> Heap -> a -> EDom
run env heap = runInterp env heap . interp

env0 :: Env
env0 = []

heap0 :: Heap
heap0 = (0,replicate frameSize (Int 0))
  where frameSize :: Int
        frameSize = 64 * 1024 -- 64 KB

runInterp0 :: IDom -> EDom
runInterp0 = runInterp env0 heap0

run0 :: Interpretable a => a -> EDom
run0 = runInterp0 . interp

----------------------------------
-- Interpreter helper functions --
----------------------------------

record :: [Dom] -> IDom
record = return . Record

int :: Int -> IDom
int = return . Int

fun :: ([Dom] -> IDom) -> IDom
fun = return . Fun

-----------------
-- Env helpers --
-----------------

letin :: Var -> Dom -> IDom -> IDom
letin x d e = local ((x,d):) e

look :: Var -> IDom
look x = do
  nv <- ask
  case lookup x nv of
    Just d -> return d
    Nothing ->
      throw $ "undefined variable " ++ show x ++ " in environment: " ++ show nv
    
clos :: Env -> [Var] -> IDom -> Dom
clos nv xs body = Fun $ \ ds -> local (const $ zip xs ds ++ nv) body

closure :: [Var] -> IDom -> IDom
closure xs body = do
  nv <- ask
  return $ clos nv xs body

func :: [Var] -> IDom -> Dom
func xs body = Fun $ \ ds -> local (const $ zip xs ds) body

function :: [Var] -> IDom -> IDom
function xs body = return $ func xs body

fix :: Fix IDom -> IDom
fix (fs,e) = do
  nv <- ask
  let nv' = funs ++ nv
      funs = map (\ (f,as,b) -> (f,clos nv' as b)) fs
  local (const nv') e

------------------
-- Heap helpers --
------------------

update :: [a] -> Int -> a -> [a]
update xs i x = take i xs ++ [x] ++ drop (i+1) xs

malloc :: Int -> InterpM Pointer
malloc i = do
  (p,hp) <- get
  put (p+i,hp)
  return p

store :: Int -> Dom -> InterpM ()
store i d = do
  (p,hp) <- get
  put (p,update hp i d)

load :: Int -> IDom
load i = do
  (_,hp) <- get
  return $ hp !! i

-------------------
-- Error Helpers --
-------------------

throw :: Error -> InterpM a
throw = lift . lift . Left
