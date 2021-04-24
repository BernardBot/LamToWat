{-# LANGUAGE TypeOperators #-}

module Tps.Test where

import Types hiding (Fix)

import Option
import Vec
import Commands
import Union

import Tps.Syntax
import Tps.Commands

e0 :: Tps (Fix :+: Base) a
e0 = 
    Node (L (Fix (("f", ["x"]) ::: Nil)))
    ((Node (R (Add (VAR "x") (INT 1))) Nil (Some ("n", Leaf (VAR "n")))) ::: Nil) (Some ("",
    Node (R (App (VAR "f") [INT 41])) Nil None))

e1 :: Tps (Fix :+: Base) a
e1 =
    Node (L (Fix (("f",["x"]) ::: Nil)))
      (Node (R (Add (VAR "x") (INT 1)))
        (Nil)
        (Some ("n",Leaf (VAR "n")))
       ::: Nil)
      (Some ("",Node (R (App (VAR "f") [INT 41]))
        (Nil)
        (None)
      ))
  
