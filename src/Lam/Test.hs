module Lam.Test where

import Lam.Syntax
import Lam.PPrinter
import Lam.Run
import Lam.Parser

es :: [Expr]
es =
  [ Num 42

  , Add (Num 13) (Num 29)

  , App (App (Lam "x" (Lam "y" (Add (Var "x") (Var "y")))) (Num 13)) (Num 29)
  ]

-- Parser Tests --

ps :: [String]
ps =
  [ "42"

  , "13 + 29"

  , "(\\ x y -> x + y) 13 29"
  ]
