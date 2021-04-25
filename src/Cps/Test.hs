{-# LANGUAGE OverloadedStrings #-}

module Cps.Test where

import Val
import Types

import Cps.Syntax

es :: [Cps]
es =
  [ DONE (INT 42)

  , RECORD [INT 42] "x" $
    SELECT 0 (VAR "x") "y" $
    DONE (VAR "y")

  , ADD (INT 13) (INT 29) "x" $
    DONE (VAR "x")

  , FIX
    [("f",["x"],
      ADD (INT 1) (VAR "x") "y" $
      DONE (VAR "y"))
    ] $
    APP (LABEL "f") [INT 41]

  , FIX
    [("f",["x"],
      ADD (INT 1) (VAR "x") "y" $
      DONE (VAR "y"))
    ,("g",["x"],
      ADD (INT 1) (VAR "x") "y" $
      APP (LABEL "f") [VAR "y"])
    ] $
    APP (LABEL "g") [INT 40]

  , RECORD [INT 1, INT 2, INT 4] "r" $
    DONE (VAR "r")

  , FIX
    [("f",["x"],
      FIX
      [("g",["y"],
        ADD (VAR "x") (VAR "y") "z" $
        DONE (VAR "z"))
      ] $
      APP (LABEL "r") [LABEL "g"])
    ,("r",["g"],
      APP (VAR "g") [INT 21])
    ] $
    APP (LABEL "f") [INT 21]

  , RECORD [INT 42] "x" $
    RECORD [VAR "x"] "y" $
    RECORD [VAR "y"] "z" $
    SELECT 0 (VAR "z") "a" $
    SELECT 0 (VAR "a") "b" $
    SELECT 0 (VAR "b") "c" $
    DONE (VAR "c")
  ]

-- Parser Tests --

ps :: [String]
ps =
  [ "return 10"

  , "x = 1 + 2\n\
    \y = x + x\n\
    \return y"

  , "x = [42]\n\
    \y = [x]\n\
    \z = [y]\n\
    \a = z[0]\n\
    \b = a[0]\n\
    \c = b[0]\n\
    \return c"
  ]
