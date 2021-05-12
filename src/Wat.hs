{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Wat where

import Text.PrettyPrint.HughesPJ
import Prelude hiding ((<>))

import Data.List

import Control.Monad

import Data.Tree

import Val
import Interpreter hiding (int)
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

instance {-# OVERLAPS #-} Interpretable Wat where
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
emit = render . pretty

emitRun :: Wat -> IO ()
emitRun wat = do
  writeFile "temp.wat" $ emit wat
  wat2wasm "temp.wat" "temp.wasm"
  wasminterp "temp.wasm"

_p = text "_p"
_t = text "_t"
_start = text "_start"

dollar = text "$"
module_ = text "module"
memory = text "memory"
mut = text "mut"
i32const = text "i32.const"
global = text "global"
i32 = text "i32"
table = text "table"
funcref = text "funcref"
elem_ = text "elem"
type_ = text "type"
func_ = text "func"
param = text "param"
export = text "export"
result = text "result"
call_indirect = text "call_indirect"
localset = text "local.set"
localget = text "local.get"
i32add = text "i32.add"
i32load = text "i32.load"
i32store = text "i32.store"
offset = text "offset"
globalget = text "global.get"
globalset = text "global.set"
local = text "local"

intSize :: Int
intSize = 4

pretty :: Wat -> Doc
pretty (fs,e) = parens (module_ $+$
  parens (memory <+> int 1) $+$

  parens (global <+> dollar <> _p <+>
          parens (mut <+> i32) <+> parens (i32const <+> int 0)) $+$

  parens (table <+> int (length fs) <+> funcref) $+$

  parens (elem_ <+> parens (i32const <+> int 0) <+>
          sep (map (\ (f,_,_) -> dollar <> text f) fs)) $+$

  vcat (map (\ (_,as,_) -> let l = length as in
               parens (type_ <+> dollar <> _t <> int l <+>
               parens (func_ <+> sep (replicate l (parens (param <+> i32))) <+>
                      parens (result <+> i32)))) fs) $+$

  parens (export <+> doubleQuotes _start <+> parens (func_ <+> dollar <> _start)) $+$

  vcat (map prettyFun ((render _start,[],e):fs)))

prettyFun :: Fun Exp -> Doc
prettyFun (f,as,b) = parens (func_ <+>
          dollar <> text f <+>

          sep (map (\ a -> parens (param <+> dollar <> text a <+> i32)) as) <+>

          parens (result <+> i32) <+>

          sep (map (\ l -> parens (local <+> dollar <> text l <+> i32))
               (nub (locals b) \\ as)) $+$

          nest 2 (prettyExp b))
  where locals (Add _ _ x e)  = x : locals e
        locals (Malloc _ x e) = x : locals e
        locals (Load _ _ x e) = x : locals e
        locals (Store _ _ _ e) = locals e
        locals e = []

prettyExp :: Exp -> Doc
prettyExp (Val v) = prettyVal v
prettyExp (App v vs) =
  parens (call_indirect <+>
          parens (type_ <+> dollar <> _t <> int (length vs)) <+>
          sep (map prettyVal vs) <+> prettyVal v)
prettyExp (Add v1 v2 x e) =
  parens (localset <+> dollar <> text x <+>
         parens (i32add <+> prettyVal v1 <+> prettyVal v2)) $+$
  prettyExp e
prettyExp (Load i v x e) =
  parens (localset <+> dollar <> text x <+>
         parens (i32load <+> offset <> equals <> int (intSize * i) <+> prettyVal v)) $+$
  prettyExp e
prettyExp (Store i s t e) =
  parens (i32store <+> offset <> equals <> int (intSize * i) <+>
         prettyVal s <+> prettyVal t) $+$
  prettyExp e
prettyExp (Malloc i x e) =
  parens (localset <+> dollar <> text x <+> parens (globalget <+> dollar <> _p)) $+$
  parens (globalset <+> dollar <> _p <+>
          parens (i32add <+> parens (globalget <+> dollar <> _p)) <+>
          parens (i32const <+> int (intSize * i))) $+$
  prettyExp e
  
prettyVal :: Val -> Doc
prettyVal (VAR x) = parens (localget <+> dollar <> text x)
prettyVal (INT i) = parens (i32const <+> int i)
prettyVal (LABEL x) = error $ "encountered LABEL value in Wat expression: " ++ show x
