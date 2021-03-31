module Val where

data Val
  = INT Int
  | VAR String
  | LABEL String

instance Show Val where
  show (INT i) = show i
  show (VAR x) = x
  show (LABEL x) = x
