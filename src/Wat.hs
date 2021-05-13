{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Wat where

import Data.Text.Prettyprint.Doc
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
emit = show . pretty

emitRun :: Wat -> IO ()
emitRun wat = do
  writeFile "temp.wat" $ emit wat
  wat2wasm "temp.wat" "temp.wasm"
  wasminterp "temp.wasm"

-- helper for variable
var x = dollar <> pretty x
vars xs = map var xs
asgn x e c = localset (var x) e <> line <> pretty c

-- operator types
unary   op arg1           = parens (pretty op <+> arg1)
binary  op arg1 arg2      = parens (pretty op <+> arg1 <+> arg2)
ternary op arg1 arg2 arg3 = parens (pretty op <+> arg1 <+> arg2 <+> arg3)

-- reserved variable names
_p = var "_p" -- heap pointer
_t = var "_t" -- type variable prefix

-- keywords
_start  = pretty "_start"
dollar  = pretty "$"
funcref = pretty "funcref"
i32     = pretty "i32"
offset  = pretty "offset"
func0   = pretty "func"

-- unary operators
func1     = unary "func"
type_     = unary "type"
mut       = unary "mut"
localget  = unary "local.get"
param     = unary "param"
local     = unary "local"
result    = unary "result"
globalget = unary "global.get"
module_ a = parens (pretty "module" <> line <> a)
export a  = binary "export" (dquotes a) (func1 (dollar <> a))

memory, i32const, table :: Int -> Doc ann
memory  = unary "memory" . pretty
i32const = unary "i32.const" . pretty
table s    = binary "table" (pretty s) funcref

-- binary operators
globalset  = binary "global.set"
localset   = binary "local.set"
i32add     = binary "i32.add"
elem_      = binary "elem"

i32load :: Int -> Doc ann -> Doc ann
i32load i  = binary "i32.load" (offset <> equals <> pretty i)

-- ternary operators
globaldef           = ternary "global"
typedef       a b c = binary "type" a (func1 (hsep (map param b) <+> result c))
call_indirect a b c = ternary "call_indirect" a (hsep c) b

i32store :: Int -> Doc ann -> Doc ann -> Doc ann
i32store i = ternary "i32.store" (offset <> equals <> pretty i)

intSize :: Int
intSize = 4

instance {-# OVERLAPS #-} Pretty Wat where
  pretty (fs,e) = module_ (vsep
    [ memory 1
    , globaldef _p (mut i32) (i32const 0)
    , table (length fs)
    , elem_ (i32const 0) (hsep (map (var . fst3) fs))
    , let lens = sort (nub (2 : map (length . snd3) fs)) in
        vsep (map (\ l ->
                typedef (_t <> pretty l) (replicate l i32) i32) lens)
    , export _start
    , vsep (map pretty ((show _start,[],e):fs))
    ])

instance {-# OVERLAPS #-} Pretty (Fun Exp) where
  pretty (f,as,b) = func1 (hsep
    [ var f
    , hsep (map (\ x -> param (var x <+> i32)) as)
    , result i32
    , hsep (map (\ x -> local (var x <+> i32)) (nub (locals b) \\ as))
    ] <> line <>
    nest 2 (pretty b))
    where locals (Add _ _ x e)  = x : locals e
          locals (Malloc _ x e) = x : locals e
          locals (Load _ _ x e) = x : locals e
          locals (Store _ _ _ e) = locals e
          locals e = []

instance Pretty Exp where
  pretty (Val v) = pretty v
  pretty (App v vs) =
    call_indirect (type_ (_t <> pretty (length vs))) (pretty v) (map pretty vs)
  pretty (Add v1 v2 x e) = asgn x (i32add (pretty v1) (pretty v2)) e
  pretty (Load i v x e) = asgn x (i32load (intSize*i) (pretty v)) e
  pretty (Store i s t e) =
   i32store (intSize * i) (pretty s) (pretty t) <> line <> pretty e
  pretty (Malloc i x e) = hsep
    [ localset (var x) (globalget _p)
    , globalset _p (i32add (globalget _p) (i32const (intSize*i)))
    , pretty e
    ]

instance Pretty Val where
    pretty (INT i) = i32const i
    pretty (VAR x) = localget (var x)
    pretty (LABEL x) = error $ "encountered LABEL value in Wat expression: " ++ show x
