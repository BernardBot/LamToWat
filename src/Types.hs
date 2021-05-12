module Types where

import Development.Shake

import Data.Tree
import Data.Tree.Pretty

import Data.Data
import Data.Generics.Aliases

import Control.Monad.State

import Text.Parsec

----------------
-- Signatures --
----------------

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

-----------------
-- Definitions --
-----------------

type Var = String
type Fun e = (Var,[Var],e)
type Fix e = ([Fun e],e)

-------------------------
-- AST Pretty Printing --
-------------------------

toTree :: Data a => a -> Tree String
toTree =
  (\ a -> Node (showConstr $ toConstr a) (gmapQ toTree a)) `extQ`
  (\ a -> Node a [])

drawAST :: Data a => a -> String
drawAST = drawTree . toTree

drawVerticalAST :: Data a => a -> String
drawVerticalAST = drawVerticalTree . toTree
