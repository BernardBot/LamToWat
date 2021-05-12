{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Types where

import Development.Shake

import Data.Tree
import Data.Tree.Pretty
import Data.List

import Control.Monad.Identity
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

python3 file = cmd_ "python3" [file]

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

---------------------
-- Pretty Printing --
---------------------

class Emitable a where
  emit :: a -> String

  emitIO :: a -> IO ()
  emitIO = putStrLn . emit

indent :: String -> String
indent = unlines . map ("  "++) . lines

assign :: String -> String -> String
assign x y = x ++ " = " ++ y ++ "\n"

args :: [String] -> String
args ss = "(" ++ intercalate "," ss ++ ")"

recs :: [String] -> String
recs ss = "[" ++ intercalate "," ss ++ "]"

parens :: String -> String
parens s = "(" ++ s ++ ")"

class Treeable a where
  toTree :: a -> Tree String

  pTree :: a -> String
  pTree = drawVerticalTree . toTree

  pTreeIO :: a -> IO ()
  pTreeIO = putStr . pTree

instance Treeable a => Treeable (Fix a) where
  toTree (fs,e) = Node "Fix"
    [Node "fs" (map (\ (f,as,b) -> Node (show f ++ " " ++ show as) [toTree b]) fs), toTree e]
