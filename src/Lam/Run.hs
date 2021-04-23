module Lam.Run where

import Types

import Lam.Syntax
import Lam.Interpreter
import Lam.Parser (parseExpr')

run :: Expr -> Dom
run = runInterp . interp

pRun :: String -> Dom
pRun = run . parseExpr'
