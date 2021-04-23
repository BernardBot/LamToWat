{-# LANGUAGE OverloadedStrings #-}

module Cps.Test where

import Types

import Cps.Syntax
import Cps.PPrinter
import Cps.Run

e0 :: Expr
e0 = DONE (INT 42)

e1 :: Expr
e1 =
  RECORD [INT 42] "x" $
  SELECT 0 (VAR "x") "y" $
  DONE (VAR "y")

e2 :: Expr
e2 =
  ADD (INT 13) (INT 29) "x" $
  DONE (VAR "x")

e3 :: Expr
e3 =
  FIX
  [("f",["x"],
     ADD (INT 1) (VAR "x") "y" $
     DONE (VAR "y"))
  ] $
  APP (LABEL "f") [INT 41]
  
e4 :: Expr
e4 =
  FIX
  [("f",["x"],
     ADD (INT 1) (VAR "x") "y" $
     DONE (VAR "y"))
  ,("g",["x"],
     ADD (INT 1) (VAR "x") "y" $
     APP (LABEL "f") [VAR "y"])
  ] $
  APP (LABEL "g") [INT 40]

    
e5 :: Expr
e5 =
  RECORD [INT 1, INT 2, INT 4] "r" $
  DONE (VAR "r")

e6 :: Expr
e6 =
  FIX
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

e7 :: Expr
e7 =
  RECORD [INT 42] "x" $
  RECORD [VAR "x"] "y" $
  RECORD [VAR "y"] "z" $
  SELECT 0 (VAR "z") "a" $
  SELECT 0 (VAR "a") "b" $
  SELECT 0 (VAR "b") "c" $
  DONE (VAR "c")
