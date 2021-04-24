{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cps.Syntax where
  
import Types

type Cps = Cps.Syntax.Expr

data Expr
  = APP Val [Val]
  | DONE Val
  | RECORD [Val] Var Expr
  | SELECT Int Val Var Expr
  | ADD Val Val Var Expr
  | FIX [Fun Expr] Expr
  deriving (Eq,Show)

instance Interpretable Expr where
  interp (FIX fs e) =
    fix (map (fmap interp) fs,interp e)
  interp (APP v vs) = do
    Fun f <- interp v
    ds <- mapM interp vs
    f ds
  interp (ADD v1 v2 x e) = do
    Int i <- interp v1
    Int j <- interp v2
    letin x (Int $ i + j) (interp e)
  interp (RECORD vs x e) = do
    ds <- mapM interp vs
    letin x (Record ds) (interp e)
  interp (SELECT i v x e) = do
    Record ds <- interp v
    letin x (ds !! i) (interp e)
  interp (DONE v) = interp v

instance PPrintable Expr where
  pprint (APP v vs)       = pprint v ++ args (map pprint vs)
  pprint (DONE v)         = "return " ++ pprint v
  pprint (ADD v1 v2 x e)  = assign x (pprint v1 ++ " + " ++ pprint v2) ++ pprint e
  pprint (RECORD vs x e)  = assign x (recs (map pprint vs)            ) ++ pprint e
  pprint (SELECT n v x e) = assign x (pprint v ++ "[" ++ show n ++ "]") ++ pprint e
  pprint (FIX fs e)       = concatMap pprint fs ++ pprint e

instance PPrintable (Fun Expr) where
  pprint (f,as,b) = "def " ++ f ++ args as ++ ":\n" ++ indent (pprint b)

