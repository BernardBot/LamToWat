module Wat where

import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

data Wat = Module [(String,[String],Exp)] Exp deriving Show

data Val
  = INT Int
  | VAR String
  deriving Show

data Exp
  = Malloc Int String Exp
  | Store Int Val Val Exp
  | Load Int Val String Exp
  | Set String Val Exp
  | Add Val Val String Exp
  | App Val [Val]
  | Done Val
  deriving Show

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
        funs = map (\ (f,as,b) vs -> if length vs /= length as then error $ show (length funs) ++ "  " ++ show (map (\(f,as,b) -> f) fs) ++ " " ++ show f ++ " " ++ show vs ++ " " ++ show as ++ "\n" ++ pprinte b else local (const $ zip as vs) $ go b) fs

        go :: Exp -> M Dom
        go (Malloc i x e) = do
          (p,hp) <- get
          put (p+i,hp)
          local ((x,p):) (go e)
        go (Store j s t e) = do
          i <- vo s
          d <- vo t
          (p,hp) <- get
          -- here you can check if we are storing in (non-)-allocated memory
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
        go w@(App v vs) = do
          p <- vo v
          ds <- mapM vo vs
          (funs !! p) ds
          -- case fs !! p of
          --   (_,as,_) | length ds /= length as -> error $ pprinte w
          --   _ -> (funs !! p) ds
        go (Done v) = vo v
        go (Set x v e) = do
          d <- vo v
          local ((x,d):) (go e)

        vo :: Val -> M Dom
        vo (VAR x) = do
          nv <- ask
          case lookup x nv of
            Just v -> return v
            Nothing -> error $ show x ++ " " ++ show nv
        vo (INT i) = return i

--------------
-- Printing --
--------------

pprintv (VAR x) = x
pprintv (INT i) = show i

indent = unlines . map ("  "++) . lines
localset x y = "localset " ++ x ++ " (" ++ y ++ ")\n"

pprinte (Add v1 v2 x e) = localset x (pprintv v1 ++ " + " ++ pprintv v2) ++ pprinte e
pprinte (App v vs) = "call " ++ pprintv v ++ concatMap ((' ':) . pprintv) vs
pprinte (Done v) = "return " ++ pprintv v
pprinte (Malloc i x e) = localset x ("malloc " ++ show i) ++ pprinte e
pprinte (Store i s t e) = "store " ++ show i ++ " " ++ pprintv s ++ " " ++ pprintv t ++ "\n" ++ pprinte e
pprinte (Load i v x e) = localset x ("load " ++ show i ++ " " ++ pprintv v) ++ pprinte e
pprinte (Set x v e) = localset x (pprintv v) ++ pprinte e

pprint (Module fs e) = "module\n" ++ concatMap pprintF fs ++ "" ++ pprinte e
  where pprintF (f,as,b) = "func " ++ f ++ concatMap (\ a -> " (param " ++ a ++ ")") as ++ "\n" ++ indent (pprinte b)

-------------
-- Testing --
-------------

e = Module [
  ("f", ["a","b"], Add (VAR "a") (VAR "b") "x" (App (INT 1) [VAR "x"])),
  ("g", ["x"], Done (VAR "x"))
  ]
  ( Store 0 (INT 10) (INT 42)
  $ Load 1 (INT 10) "x"
  $ Done (VAR "x"))


e2 = Module 
  [("f",["x"],Add (VAR "x") (INT 1) "r" (Done (VAR "r")))]
  (App (INT 0) [INT 41])
