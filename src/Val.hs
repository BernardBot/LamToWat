module Val where

import Data.Tree
import Data.Char
import Data.String
import Data.Maybe
import Data.List

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

instance Emitable Val where
  emit (INT i) = show i
  emit (VAR x) = x
  emit (LABEL x) = x

instance Treeable Val where
  toTree v = Node (show v) []

instance IsString Val where
  fromString "" = VAR "" -- INT 0 ?
  fromString s@(x:xs)
    | isDigit x = INT $ read s
    | x == '$'  = LABEL s
    | otherwise = VAR s
