module Types where

import Development.Shake

import Data.Tree
import Data.Tree.Pretty
import Data.List

import Control.Monad.Identity
import Control.Monad.State

import Text.Parsec

-----------------
-- Definitions --
-----------------

type Var = String
type Fun e = (Var,[Var],e)
type Fix e = ([Fun e],e)

data Nat = Z | S Nat
type Sig = Nat -> Bool -> * -> * -> * -> *

----------
-- Util --
----------

wasminterp file = cmd_ bin [file, "--run-all-exports"]
  where bin = "/Users/ben/wabt/bin/wasm-interp"

wat2wasm file outFile = cmd_ bin [file, "--output=" ++ outFile]
  where bin = "/Users/ben/wabt/bin/wat2wasm"

fresh s = do
  i <- get
  put (i+1)
  return $ "_" ++ s ++ show i

runFresh = fst . flip runState 0

-------------------
-- Tree Printing --
-------------------

class Treeable a where
  toTree :: a -> Tree String

  pTree :: a -> String
  pTree = drawVerticalTree . toTree

  pTreeIO :: a -> IO ()
  pTreeIO = putStr . pTree
