{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Wat.Syntax where

import Data.List

import Control.Monad

import Types

type Wat = Expr
type Expr = Fix Exp

type Offset = Int
data Exp
  = Malloc Int Var Exp
  | Store Offset Val Val Exp
  | Load Offset Val Var Exp
  | Add Val Val Var Exp
  | App Val [Val]
  | Done Val
  deriving (Eq,Show)

instance Interpretable Expr where
  interp (fs,e) = do
    malloc (length fs)
    zipWithM_
      (\ (f,as,b) i -> do
          f <- function as (interp b)
          store i f
      ) fs [0..]
    interp e
        
instance Interpretable Exp where
  interp (Malloc i x e) = do
    p <- malloc i
    letin x (Int p) (interp e)
  interp (Store i s t e) = do
    Int j <- interp s
    d     <- interp t
    store (i+j) d
    interp e
  interp (Load i v x e) = do
    Int j <- interp v
    d <- load (i+j)
    letin x d (interp e)
  interp (Add v1 v2 x e) = do
    Int i <- interp v1
    Int j <- interp v2
    letin x (Int $ i + j) (interp e)
  interp (App v vs) = do
    Int fp <- interp v
    Fun f  <- load fp
    ds     <- mapM interp vs
    f ds
  interp (Done v) = interp v

instance PPrintable Expr where
  pprint (fs,e) =
    "(module\n" ++
    "(memory 1)\n" ++
    "(global $" ++ _p ++ " (mut i32) (i32.const 0))\n" ++
    "(table " ++ show (length fs) ++ " funcref)\n" ++
    "(elem (i32.const 0)" ++ concatMap (\ name -> " $" ++ name) names ++ ")\n" ++
    types ++
    "(export \"" ++ _start ++ "\" (func $" ++ _start ++ "))\n" ++
    funcs ++ ")"
    where names = map (\ (f,_,_) -> f) fs
          funcs = concatMap pprint ((_start,[],e):fs)
          lengths = sort (nub (2 : (map (\ (_,as,_) -> length as) fs)))
          types = concatMap typedef lengths

          typedef :: Int -> String
          typedef len = "(type $" ++ _t ++ show len ++ " (func " ++ spaced (replicate len "(param i32)") ++ " (result i32)))\n"
          
instance PPrintable (Fun Exp) where
  pprint (f,as,b) =
    "(func $" ++ f ++ " " ++ params as ++ " (result i32) " ++ locals ls ++ "\n" ++ indent (pprint b) ++ ")\n"
    where param p = "(param $" ++ p ++ " i32)"
          local l = "(local $" ++ l ++ " i32)"
          params = spaced . map param
          locals = spaced . map local
          ls = nub (lv b) \\ as
          
          lv (Add _ _ x e)  = x : lv e
          lv (Malloc _ x e) = x : lv e
          lv (Load _ _ x e) = x : lv e
          lv (Store _ _ _ e) = lv e
          lv e = []

instance PPrintable Exp where
  pprint (Done v)        = pprintV v
  pprint (App v vs)      = "(call_indirect (type $" ++ _t ++ show (length vs) ++ ") " ++ sp vs ++ " " ++ pprintV v ++ ")"
  pprint (Add v1 v2 x e) = "(local.set $" ++ x ++ " (i32.add " ++ pprintV v1 ++ " " ++ pprintV v2 ++"))\n" ++ pprint e
  pprint (Load i v x e)  = "(local.set $" ++ x ++ " (i32.load offset=" ++ show (intSize * i) ++ " " ++ pprintV v ++ "))\n" ++ pprint e
  pprint (Store i s t e) = "(i32.store offset=" ++ show (intSize * i) ++ " " ++ pprintV s ++ " " ++ pprintV t ++ ")\n" ++ pprint e
  pprint (Malloc i x e)  =
    "(local.set $" ++ x ++ " (global.get $" ++ _p ++ "))\n" ++
    "(global.set $" ++ _p ++ " (i32.add (global.get $" ++ _p ++ ") (i32.const " ++ show (intSize * i) ++ ")))\n" ++
    pprint e

pprintV :: Val -> String
pprintV (VAR x) = "(local.get $" ++ x ++ ")"
pprintV (INT i) = "(i32.const " ++ show i ++ ")"
pprintV (LABEL x) = error $ "encountered LABEL value in Wat expression: " ++ show x

_p,_t,_start :: String
_p = "_p"
_t = "_t"
_start = "_start"

intSize :: Int
intSize = 4

spaced :: [String] -> String
spaced = intercalate " "

sp :: [Val] -> String
sp = spaced . map pprintV
