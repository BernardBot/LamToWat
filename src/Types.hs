{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Data.String
import Data.Char
import Data.Maybe
import Data.List

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Text.Parsec
import Text.Parsec.Indent

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
type MonadP = IndentT Identity
type Parser a = ParsecT StreamP UserStateP MonadP a 

class Parsable a where
  parseExpr :: StreamP -> Either ParseError a

  parseExpr' :: StreamP -> a
  parseExpr' s = case parseExpr s of
    Right exp -> exp
    Left err -> error $ show err

---------------------
-- Transformations --
---------------------

class Transformable a b where
  transform :: a -> b

instance Transformable a b => Transformable [a] [b] where
  transform = map transform

-----------------
-- Definitions --
-----------------

type Var = String
type Fun e = (Var,[Var],e)
type Fix e = ([Fun e],e)

data Val
  = INT Int
  | VAR Var
  | LABEL Var
  deriving (Eq,Show)

instance Interpretable Val where
  interp (INT i) = int i
  interp (VAR x) = look x
  interp (LABEL x) = look x

instance PPrintable Val where
  pprint (INT i) = show i
  pprint (VAR x) = x
  pprint (LABEL x) = x

instance IsString Val where
  fromString "" = VAR "" -- INT 0 ?
  fromString s@(x:xs)
    | isDigit x = INT $ read s
    | x == '$'  = LABEL s
    | otherwise = VAR s

-----------------
-- Interpreter --
-----------------

type Env = [(Var,Dom)]
type Pointer = Int
type Heap = (Pointer,[Dom])
type Interp = StateT Heap (ReaderT Env Maybe)
type IDom = Interp Dom

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

class Interpretable a where
  interp :: a -> IDom

-- Run interpreter --

runInterp :: Env -> Heap -> IDom -> Dom
runInterp env heap =
  fst .
  fromJust . -- Error handling?
  flip runReaderT env .
  flip runStateT heap

run :: Interpretable a => Env -> Heap -> a -> Dom
run env heap = runInterp env heap . interp

env0 :: Env
env0 = []

heap0 :: Heap
heap0 = (0,replicate frameSize (Int 0))
  where frameSize :: Int
        frameSize = 64 * 1024 -- 64 KB

runInterp0 :: IDom -> Dom
runInterp0 = runInterp env0 heap0

run0 :: Interpretable a => a -> Dom
run0 = runInterp0 . interp

-- Interpreter helper functions --

record :: [Dom] -> IDom
record = return . Record

int :: Int -> IDom
int = return . Int

fun :: ([Dom] -> IDom) -> IDom
fun = return . Fun

-- Env helpers --

letin :: Var -> Dom -> IDom -> IDom
letin x d e = local ((x,d):) e

look :: Var -> IDom
look x = do
  nv <- ask
  case lookup x nv of
    Just d -> return d
    Nothing ->
      error $ "undefined variable " ++ show x ++ " in environment: " ++ show nv
    
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

-- Heap helpers --

update :: [a] -> Int -> a -> [a]
update xs i x = take i xs ++ [x] ++ drop (i+1) xs

malloc :: Int -> Interp Pointer
malloc i = do
  (p,hp) <- get
  put (p+i,hp)
  return p

store :: Int -> Dom -> Interp ()
store i d = do
  (p,hp) <- get
  put (p,update hp i d)

load :: Int -> IDom
load i = do
  (_,hp) <- get
  return $ hp !! i

---------------------
-- Pretty Printing --
---------------------

class PPrintable a where
  pprint :: a -> String

  pprintIO :: a -> IO ()
  pprintIO = putStrLn . pprint

instance PPrintable a => PPrintable [a] where
  pprint = recs . map pprint

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
