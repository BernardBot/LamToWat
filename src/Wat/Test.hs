module Wat.Test where

import Types

import Wat.Syntax

e0 :: Expr
e0 =
  ([("f",["x"],
     Add (VAR "x") (INT 1) "r" $
     Done (VAR "r"))
   ],
  App (INT 0) [INT 41])


-- Expressions --

ex0 :: Exp
ex0 = Done (INT 42)

ex1 :: Exp
ex1 =
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Malloc 7 "x" $
  Done (VAR "x")

ex2 :: Exp
ex2 =
  Store 0 (INT 0) (INT 42) $
  Load 0 (INT 0) "x" $
  Done (VAR "x")

ex3 :: Exp
ex3 =
  Store 0 (INT 0) (INT 13) $
  Store 0 (INT 0) (INT 42) $
  Load 0 (INT 0) "x" $
  Done (VAR "x")

ex4 :: Exp
ex4 =
  Store 0 (INT 0) (INT 13) $
  Store 10 (INT 0) (INT 42) $
  Load 0 (INT 0) "x" $
  Done (VAR "x")

ex5 :: Exp
ex5 =
  Store 10 (INT 0) (INT 13) $
  Store 0 (INT 0) (INT 42) $
  Load 0 (INT 0) "x" $
  Done (VAR "x")

ex6 :: Exp
ex6 =
  Store 10 (INT 0) (INT 13) $
  Store 0 (INT 0) (INT 42) $
  Load 10 (INT 0) "x" $
  Done (VAR "x")
  
