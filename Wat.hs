module Wat where

import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

data Val
  = INT Int
  | VAR String

data Wat = Module [(String,[String],Exp)] Exp

data Exp
  = Malloc Int String Exp
  | Store Int Val Val Exp
  | Load Int Val String Exp
  | Add Val Val String Exp
  | App Val [Val]
  | Done Val

type Dom = Int
type Fun = [Dom] -> M Dom

type Env = [(String,Dom)]
type Heap = (Pointer,[Dom])
type Pointer = Int
type M = StateT Heap (ReaderT Env Maybe)

-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Memory
frameSize :: Int
frameSize = 64 * 1024 -- 64 KB

wat2dom :: Wat -> Dom
wat2dom =
  fst .
  fromJust .
  flip runReaderT [] .
  flip runStateT (0,replicate frameSize 0) .
  w2d

w2d :: Wat -> M Dom
w2d (Module fs e) = go e
  where funs :: [Fun]
        funs = map (\ (f,as,b) vs -> local (const (zip as vs)) (go b)) fs

        go :: Exp -> M Dom
        go (Malloc i x e) = do
          (p,hp) <- get
          put (p+i,hp)
          local ((x,p):) (go e)
        go (Store j s t e) = do
          i <- vo s
          d <- vo t
          (p,hp) <- get
          let (a,_:b) = splitAt (i+j) hp
          put (p,a ++ d : b)
          go e
        go (Load i v x e) = do
          (_,hp) <- get
          p <- vo v
          local ((x,hp !! (p + i)):) (go e)
        go (Add v1 v2 x e) = do
          i1 <- vo v1
          i2 <- vo v2
          local ((x,i1 + i2):) (go e)
        go (App v vs) = do
          p <- vo v
          ds <- mapM vo vs
          (funs !! p) ds
        go (Done v) = vo v

        vo :: Val -> M Dom
        vo (VAR x) = do
          nv <- ask
          Just v <- return (lookup x nv)
          return v
        vo (INT i) = return i

---------------------
-- Pretty Printing --
---------------------

instance Show Val where
  show (VAR x) = x
  show (INT i) = show i

indent = unlines . map ("  "++) . lines
assign x y = x ++ " = " ++ y ++ "\n"

instance Show Wat where
  show (Module fs e) = "module\n" ++ concatMap showF fs ++ "" ++ show e
    where showF (f,as,b) = "func " ++ f ++ concatMap (\ a -> " (param " ++ a ++ ")") as ++ "\n" ++ indent (show b)

instance Show Exp where
  show (Done v) = "return " ++ show v
  show (App v vs) = "call " ++ show v ++ concatMap ((' ':) . show) vs
  show (Add v1 v2 x e) = assign x (show v1 ++ " + " ++ show v2) ++ show e
  show (Malloc i x e) = assign x ("malloc " ++ show i) ++ show e
  show (Load i v x e) = assign x ("load " ++ show i ++ " " ++ show v) ++ show e
  show (Store i s t e) = "store " ++ show i ++ " " ++ show s ++ " " ++ show t ++ "\n" ++ show e

