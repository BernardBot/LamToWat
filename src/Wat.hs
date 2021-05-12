{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Wat where

import Text.PrettyPrint.HughesPJ
import Prelude hiding ((<>))

import Data.List

import Control.Monad

import Data.Tree

import Val
import Interpreter hiding (int,func)
import Types

type Wat = Fix Exp

type Offset = Int
data Exp
  = Malloc Int Var Exp
  | Store Offset Val Val Exp
  | Load Offset Val Var Exp
  | Add Val Val Var Exp
  | App Val [Val]
  | Val Val
  deriving (Eq,Show)

instance Interpretable Wat where
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
  interp (Val v) = interp v

instance Treeable Wat where
  toTree (fs,e) =  Node "Fix"
    [Node "fs"
     (map (\ (f,as,b) -> Node (show f ++ " " ++ show as) [toTree b]) fs), toTree e]

instance Treeable Exp where
  toTree (Malloc i x e) =
    Node (x ++ " = Malloc " ++ show i) [toTree e]
  toTree (Store i s t e) =
    Node ("Store " ++ show i ++ " (" ++ show s ++ ") (" ++ show t ++ ")") [toTree e]
  toTree (Load i v x e) =
    Node (x ++ " = Load " ++ show i ++ " (" ++ show v ++ ") ") [toTree e]
  toTree (Add v1 v2 x e) =
    Node (x ++ " = Add (" ++ show v1 ++ ") (" ++ show v2 ++ ")") [toTree e]
  toTree (App v vs) = Node ("App (" ++ show v ++ ") " ++ show vs) []
  toTree (Val v) = toTree v

emit :: Wat -> String
emit = render . pWat

emitRun :: Wat -> IO ()
emitRun wat = do
  writeFile "temp.wat" $ emit wat
  wat2wasm "temp.wat" "temp.wasm"
  wasminterp "temp.wasm"

var x = dollar <> text x
sexp fun args = parens (text fun <+> args)

_p = text "_p"
_t = text "_t"
_start = text "_start"
dollar = text "$"
funcref = text "funcref"
i32 = text "i32"
offset = text "offset"
func_ = text "func"

func = sexp "func"
module_ defs = parens (text "module" $+$ defs)
memory = sexp "memory"
mut = sexp "mut"
globaldef var m val = sexp "global" (var <+> m <+> val)
table size typ = sexp "table" (size <+> typ)
elem_ off names = sexp "elem" (off <+> hsep names)
typedef name params res = typ (name <+> func (hsep (map param params) <+> result res))
typ = sexp "type"
export exportName name =
  sexp "export" (doubleQuotes name <+> func name)
call_indirect typ fun args = sexp "call_indirect" (typ <+> hsep args <+> fun)
i32const = sexp "i32.const"
i32add x y = sexp "i32.add" (x <+> y)
i32load i x = sexp "i32.load" (offset <> equals <> i <+> x)
i32store i s t = sexp "i32.store" (offset <> equals <> i <+> s <+> t)
globalset a b = sexp "global.set" (a <+> b)
globalget = sexp "global.get"
localset a b = sexp "local.set" (a <+> b)
localget = sexp "local.get"
param = sexp "param"
local = sexp "local"
result = sexp "result"

intSize :: Int
intSize = 4

pWat :: Wat -> Doc
pWat (fs,e) = module_ (
  memory (int 1) $+$
  globaldef (dollar <> _p) (mut i32) (i32const (int 0)) $+$
  table (int (length fs)) funcref $+$
  elem_ (i32const (int 0)) names $+$
  vcat (map (\ l -> typedef
              (dollar <> _t <> int l) (replicate l i32) i32) lengths) $+$
  export _start (dollar <> _start) $+$
  vcat (map pFun ((render _start,[],e):fs)))
  where names = map (\ (f,_,_) -> var f) fs
        lengths = sort (nub (2 : map (\ (_,as,_) -> length as) fs))

pFun :: Fun Exp -> Doc
pFun (name,as,body) = parens (
  func_ <+>
  var name <+>
  hsep (map (\ x -> param (var x <+> i32)) as) <+>
  result i32 <+>
  hsep (map (\ x -> local (var x <+> i32)) (nub (locals body) \\ as)) $+$
  nest 2 (pExp body))
  where locals (Add _ _ x e)  = x : locals e
        locals (Malloc _ x e) = x : locals e
        locals (Load _ _ x e) = x : locals e
        locals (Store _ _ _ e) = locals e
        locals e = []

pExp :: Exp -> Doc
pExp (Val v) = pVal v
pExp (App v vs) = call_indirect
  (typ (dollar <> _t <> int (length vs)))
  (pVal v)
  (map pVal vs)
pExp (Add v1 v2 x e) =
  localset (var x) (i32add (pVal v1) (pVal v2)) $+$
  pExp e
pExp (Load i v x e) =
  localset (var x) (i32load (int (intSize * i)) (pVal v)) $+$
  pExp e
pExp (Store i s t e) =
  i32store (int (intSize * i)) (pVal s) (pVal t) $+$
  pExp e
pExp (Malloc i x e) =
  localset (var x) (globalget (dollar <> _p)) $+$
  globalset (dollar <> _p)
    (i32add (globalget (dollar <> _p)) (i32const (int (intSize * i)))) $+$
  pExp e

pVal :: Val -> Doc
pVal (VAR x) = localget (var x)
pVal (INT i) = i32const (int i)
pVal (LABEL x) = error $ "encountered LABEL value in Wat expression: " ++ show x
