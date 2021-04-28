{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cont.Cps where

import Data.Tree

import Val
import Interpreter hiding (Record)
import qualified Interpreter
import Types

data Cps
  = App Val [Val]
  | Val Val
  | Record [Val] Var Cps
  | Select Int Val Var Cps
  | Add Val Val Var Cps
  | Fix [Fun Cps] Cps
  deriving (Eq,Show)

instance Treeable Cps where
  toTree (Fix fs e) = toTree (fs,e)
  toTree (Record vs x e) = Node (x ++ " = Record " ++ show vs) [toTree e]
  toTree (Select i v x e) = Node (x ++ " = Select " ++ show i ++ " (" ++ show v ++ ")") [toTree e]
  toTree (Add v1 v2 x e) = Node (x ++ " = Add (" ++ show v1 ++ ") (" ++ show v2 ++ ")") [toTree e]
  toTree (App v vs) = Node ("App (" ++ show v ++ ") " ++ show vs) []
  toTree (Val v) = toTree v

instance Interpretable Cps where
  interp (Fix fs e) =
    fix (map (fmap interp) fs,interp e)
  interp (App v vs) = do
    Fun f <- interp v
    ds <- mapM interp vs
    f ds
  interp (Add v1 v2 x e) = do
    Int i <- interp v1
    Int j <- interp v2
    letin x (Int $ i + j) (interp e)
  interp (Record vs x e) = do
    ds <- mapM interp vs
    letin x (Interpreter.Record ds) (interp e)
  interp (Select i v x e) = do
    Interpreter.Record ds <- interp v
    letin x (ds !! i) (interp e)
  interp (Val v) = interp v

instance Emitable Cps where
  emit (App v vs)       = "return " ++ emit v ++ args (map emit vs)
  emit (Val v)          = "return " ++ emit v
  emit (Add v1 v2 x e)  = assign x (emit v1 ++ " + " ++ emit v2)    ++ emit e
  emit (Record vs x e)  = assign x (recs (map emit vs)            ) ++ emit e
  emit (Select n v x e) = assign x (emit v ++ "[" ++ show n ++ "]") ++ emit e
  emit (Fix fs e)       = concatMap emit fs ++ emit e

instance Emitable (Fun Cps) where
  emit (f,as,b) = "def " ++ f ++ args as ++ ":\n" ++ indent (emit b)

emitRun :: Cps -> IO ()
emitRun cps = do
  writeFile "cps_temp.py" cps'
  python3 "cps_temp.py"
  where foo = lines $ emit cps
        baz = "print(" ++ drop (length "return ") (last foo) ++ ")"
        cps' = unlines $ init foo ++ [baz]
