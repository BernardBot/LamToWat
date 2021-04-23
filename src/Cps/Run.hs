module Cps.Run where

import Types

import Cps.Syntax
import Cps.Interpreter

run :: Expr -> Dom
run = runInterp . interp
