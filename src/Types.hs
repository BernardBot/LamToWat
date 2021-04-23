module Types where

import Data.String
import Data.Char (isDigit)
import Data.Maybe
import Data.List

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Text.Parsec
import Text.Parsec.Indent

----------
-- Util --
----------

fresh s = do
  i <- get
  put (i+1)
  return $ "_" ++ s ++ show i

------------
-- Parser --
------------

type StreamP = String
type UserStateP = ()
type MonadP = IndentT Identity

type Parser a = ParsecT StreamP UserStateP MonadP a 

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

instance IsString Val where
  fromString "" = VAR "" -- INT 0 ?
  fromString s@(x:xs)
    | isDigit x = INT $ read s
    | x == '$'  = LABEL s
    | otherwise = VAR s

pprintV :: Val -> String
pprintV (INT i) = show i
pprintV (VAR x) = x
pprintV (LABEL x) = x

interpV :: Val -> IDom
interpV (INT i) = int i
interpV (VAR x) = look x
interpV (LABEL x) = look x

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

-- Run interpreter --

env0 :: Env
env0 = []

heap0 :: Heap
heap0 = (0,replicate frameSize (Int 0))
  where frameSize :: Int
        frameSize = 64 * 1024 -- 64 KB

runInterp :: IDom -> Dom
runInterp =
  fst .
  fromJust . -- Error handling?
  flip runReaderT env0 .
  flip runStateT heap0

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
