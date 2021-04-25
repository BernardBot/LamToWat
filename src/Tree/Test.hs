module Tree.Test where

import Val
import Types

import Tree.Syntax

import Vec
import Option
import Commands
import Union

e0 :: Tree LamCmd Val
e0 = Node (R (L (Fix (("f", ["x"]) ::: Nil)))) ((\ () ->
      Node (R (R (Add (VAR "x") (INT 1)))) Nil (Some (\ n -> Leaf n))) ::: Nil) (Some (\ () ->
    Node (R (R (App (VAR "f") [INT 41]))) Nil None))

