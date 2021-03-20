module Val where

data Val
  = INT Int
  | VAR String
  | LABEL String
  deriving Show
