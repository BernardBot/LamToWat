{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Wat.Test where

import Types

import Wat.Syntax
import Wat.Interpreter
import Wat.PPrinter

run :: Expr -> Dom
run = runInterp . interp

runExp :: Exp -> Dom
runExp = run . ([],)

allExp = [e0,e1,e2,e3,e4,e5,e6]

e0 :: Exp
e0 = Done (INT 42)

e1 :: Exp
e1 =
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Done (VAR "x")

e2 :: Exp
e2 =
  Store 0 (INT 0) "42" $
  Load 0 (INT 0) "x" $
  Done (VAR "x")

e3 :: Exp
e3 =
  Store 0 (INT 0) (INT 13) $
  Store 0 (INT 0) (INT 42) $
  Load 0 (INT 0) "x" $
  Done (VAR "x")

e4 :: Exp
e4 =
  Store 0 (INT 0) (INT 13) $
  Store 10 (INT 0) (INT 42) $
  Load 0 (INT 0) "x" $
  Done (VAR "x")

e5 :: Exp
e5 =
  Store 10 (INT 0) (INT 13) $
  Store 0 (INT 0) (INT 42) $
  Load 0 (INT 0) "x" $
  Done (VAR "x")

e6 :: Exp
e6 =
  Store 10 (INT 0) (INT 13) $
  Store 0 (INT 0) (INT 42) $
  Load 10 (INT 0) "x" $
  Done (VAR "x")
  
