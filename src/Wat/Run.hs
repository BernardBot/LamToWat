{-# LANGUAGE TupleSections #-}

module Wat.Run where

import Types

import Wat.Interpreter
import Wat.Syntax

run :: Expr -> Dom
run = runInterp . interp

runExp :: Exp -> Dom
runExp = run . ([],)
