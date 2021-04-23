module Wat.PPrinter where

import Data.List

import Types hiding (pprintV,func)

import Wat.Syntax

pprint :: Expr -> String
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
          funcs = concatMap func ((_start,[],e):fs)
          lengths = sort (nub (2 : (map (\ (_,as,_) -> length as) fs)))
          types = concatMap typedef lengths

          typedef :: Int -> String
          typedef len = "(type $" ++ _t ++ show len ++ " (func " ++ spaced (replicate len "(param i32)") ++ " (result i32)))\n"
          
          func :: Fun Exp -> String
          func (f,as,b) =
            "(func $" ++ f ++ " " ++ params as ++ " (result i32) " ++ locals ls ++ "\n" ++ indent (pprintExp b) ++ ")\n"
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

pprintExp :: Exp -> String
pprintExp (Done v)        = pprintV v
pprintExp (App v vs)      = "(call_indirect (type $" ++ _t ++ show (length vs) ++ ") " ++ sp vs ++ " " ++ pprintV v ++ ")"
pprintExp (Add v1 v2 x e) = "(local.set $" ++ x ++ " (i32.add " ++ pprintV v1 ++ " " ++ pprintV v2 ++"))\n" ++ pprintExp e
pprintExp (Load i v x e)  = "(local.set $" ++ x ++ " (i32.load offset=" ++ show (intSize * i) ++ " " ++ pprintV v ++ "))\n" ++ pprintExp e
pprintExp (Store i s t e) = "(i32.store offset=" ++ show (intSize * i) ++ " " ++ pprintV s ++ " " ++ pprintV t ++ ")\n" ++ pprintExp e
pprintExp (Malloc i x e)  =
  "(local.set $" ++ x ++ " (global.get $" ++ _p ++ "))\n" ++
  "(global.set $" ++ _p ++ " (i32.add (global.get $" ++ _p ++ ") (i32.const " ++ show (intSize * i) ++ ")))\n" ++
  pprintExp e

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

