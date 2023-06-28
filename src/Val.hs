module Val where

import Data.Text.Prettyprint.Doc

import Data.Tree
import Data.Data

import Types
import Interpreter

data Val
  = INT Int
  | VAR Var
  | LABEL Var
  deriving (Eq,Show)

instance Interpretable Val where
  interp (INT i) = int i
  interp (VAR x) = look x
  interp (LABEL x) = look x

instance Treeable Val where
  toTree v = Node (show v) []

-- instance {-# OVERLAPPABLE #-} Pretty Val where
--   pretty (INT i) = pretty i
--   pretty (VAR x) = pretty x
--   pretty (LABEL x) = pretty x
