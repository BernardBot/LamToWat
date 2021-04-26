{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

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
fresh s = do
  i <- get
  put (i+1)
  return $ "_" ++ s ++ show i

runFresh = fst . flip runState 0

------------
-- Parser --
------------

type StreamP = String
type UserStateP = ()
type MonadP = Identity
type Parser a = ParsecT StreamP UserStateP MonadP a 

-----------------
-- Definitions --
-----------------

type Var = String
type Fun e = (Var,[Var],e)
type Fix e = ([Fun e],e)

---------------------
-- Pretty Printing --
---------------------

class PPrintable a where
  pprint :: a -> String

  pprintIO :: a -> IO ()
  pprintIO = putStrLn . pprint

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
