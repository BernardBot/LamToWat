module Wat where

import Val

import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.List

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

wat2int :: Wat -> Int
wat2int = wat2dom

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
          case lookup x nv of
            Just v -> return v
            Nothing -> error $ x ++ " " ++ show nv ++ " " ++ show (map (\ (f,_,_) -> f) fs)
        vo (INT i) = return i

---------------------
-- Pretty Printing --
---------------------

_p = "_p"
_t = "_t"
_start = "_start"

intSize = 4

spaced = intercalate " "
sp = spaced . map show
indent = unlines . map ("  "++) . lines

lv :: Exp -> [String]
lv (Add _ _ x e)  = x : lv e
lv (Malloc _ x e) = x : lv e
lv (Load _ _ x e) = x : lv e
lv (Store _ _ _ e) = lv e
lv e = []

pprint (VAR x) = "(local.get $" ++ x ++ ")"
pprint (INT i) = "(i32.const " ++ show i ++ ")"

instance Show Wat where
  show (Module fs e) =
    "(module\n" ++
    "(memory 1)\n" ++
    "(global $" ++ _p ++ " (mut i32) (i32.const 0))\n" ++
    "(table " ++ show (length fs) ++ " funcref)\n" ++
    "(elem (i32.const 0)" ++ concatMap (\ name -> " $" ++ name) names ++ ")\n" ++
    types ++
    "(export \"" ++ _start ++ "\" (func $" ++ _start ++ "))\n" ++
    funcs ++ ")"
    where names = map (\ (f,_,_) -> f) fs
          funcs = concatMap func ((_start,[],e):fs)
          lengths = sort (nub (map (\ (_,as,_) -> length as) fs))
          types = concatMap typedef lengths

typedef len = "(type $" ++ _t ++ show len ++ " (func " ++ spaced (replicate len "(param i32)") ++ " (result i32)))\n"

func (f,as,b) =
  "(func $" ++ f ++ " " ++ params as ++ " (result i32) " ++ locals ls ++ "\n" ++ indent (show b) ++ ")\n"
  where param p = "(param $" ++ p ++ " i32)"
        local l = "(local $" ++ l ++ " i32)"
        params = spaced . map param
        locals = spaced . map local
        ls = nub (lv b) \\ as

instance Show Exp where
  show (Done v) = pprint v
  show (App v vs) = "(call_indirect (type $" ++ _t ++ show (length vs) ++ ") " ++ sp vs ++ " " ++ pprint v ++ ")"
  show (Add v1 v2 x e) = "(local.set $" ++ x ++ " (i32.add " ++ pprint v1 ++ " " ++ pprint v2 ++"))\n" ++ show e
  show (Malloc i x e) = "(local.set $" ++ x ++ " (global.get $" ++ _p ++ "))\n" ++
    "(global.set $" ++ _p ++ " (i32.add (global.get $" ++ _p ++ ") (i32.const " ++ show (intSize * i) ++ ")))\n" ++
    show e
  show (Load i v x e) = "(local.set $" ++ x ++ " (i32.load offset=" ++ show (intSize * i) ++ " " ++ pprint v ++ "))\n" ++ show e
  show (Store i s t e) = "(i32.store offset=" ++ show (intSize * i) ++ " " ++ pprint s ++ " " ++ pprint t ++ ")\n" ++ show e


e = Module 
  [("f",["x"],Add (VAR "x") (INT 1) "r" (Done (VAR "r")))]
  (App (INT 0) [INT 41])
