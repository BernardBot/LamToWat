module Cps where
  
import Data.Maybe
import Data.List
import Control.Monad.Reader

data Val
  = INT Int
  | VAR String
  | LABEL String
  
type Fun = (String, [String], Cps)
data Cps
  = APP Val [Val]
  | DONE Val
  | RECORD [Val] String Cps
  | SELECT Int Val String Cps
  | ADD Val Val String Cps
  | FIX [Fun] Cps

-----------------
-- Interpreter --
-----------------

type Env = [(String,Dom)]
type M = ReaderT Env Maybe
data Dom
  = Record [Dom]
  | Int Int
  | Fun ([Dom] -> M Dom)

instance Show Dom where
  show (Record ds) = show ds
  show (Int i) = show i
  show (Fun _) = "fun"

cps2dom :: Cps -> Dom
cps2dom = fromJust . flip runReaderT [] . c2d
  
c2d :: Cps -> M Dom
c2d (FIX fs e) = do
  nv <- ask
  let nv' = zip (map (\ (f,as,b) -> f) fs) funs ++ nv
      funs = map (\ (f,as,b) ->
                    Fun (\ vs -> local (const (zip as vs ++ nv')) (c2d b))) fs
  local (const nv') (c2d e)
c2d (APP v vs) = do
  Fun f <- v2d v
  ds <- mapM v2d vs
  f ds
c2d (ADD v1 v2 x e) = do
  Int i1 <- v2d v1
  Int i2 <- v2d v2
  local ((x,Int (i1 + i2)):) (c2d e)
c2d (RECORD vs x e) = do
  ds <- mapM v2d vs
  local ((x,Record ds):) (c2d e)
c2d (SELECT i v x e) = do
  Record ds <- v2d v
  local ((x,ds !! i):) (c2d e)
c2d (DONE v) = v2d v

v2d (INT i) = return (Int i)
v2d (VAR x) = ReaderT (lookup x)
v2d (LABEL x) = ReaderT (lookup x)

---------------------
-- Pretty Printing --
---------------------

instance Show Val where
  show (VAR x) = x
  show (LABEL x) = "_" ++ x
  show (INT i) = show i

tab = "  "
indent = unlines . map (tab++) . lines
assign x y = x ++ " = " ++ y ++ "\n"
args ss = "(" ++ intercalate "," ss ++ ")"

instance Show Cps where
  show (ADD v1 v2 x e)  = assign x (show v1 ++ " + " ++ show v2) ++ show e
  show (RECORD vs x e)  = assign x (show vs                    ) ++ show e
  show (SELECT n v x e) = assign x (show v ++ " !! " ++ show n ) ++ show e
  show (APP v vs)       = show v ++ args (map show vs)
  show (DONE v)         = show v
  show (FIX fs e)       = concatMap showF fs                     ++ show e
    where showF (f,as,b) = "def " ++ f ++ args as ++ ":\n" ++ indent (show b)
