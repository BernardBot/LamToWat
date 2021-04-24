module Lam.Test where

import Types
import Lam.Syntax
import Lam.Parser

es :: [Expr]
es =
  [ Val (INT 42)

  , Add (Val (INT 13)) (Val (INT 29))

  , App (App (Lam "x" (Lam "y" (Add (Val (VAR "x")) (Val (VAR "y"))))) (Val (INT 13))) (Val (INT 29))
  ]

-- Parser Tests --

ps :: [String]
ps =
  [ "42"

  , "13 + 29"

  , "(\\ x y -> x + y) 13 29"
  ]
