module Cps.Run where

import Types

import Cps.Syntax
import Cps.Interpreter
import Cps.Parser (parseExpr')

run :: Expr -> Dom
run = runInterp . interp

pRun :: String -> Dom
pRun = run . parseExpr'
