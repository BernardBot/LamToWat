module Val where

import Data.Char
import Data.String

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

instance PPrintable Val where
  pprint (INT i) = show i
  pprint (VAR x) = x
  pprint (LABEL x) = x

instance IsString Val where
  fromString "" = VAR "" -- INT 0 ?
  fromString s@(x:xs)
    | isDigit x = INT $ read s
    | x == '$'  = LABEL s
    | otherwise = VAR s
