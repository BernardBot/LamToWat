module Main where

import System.Environment

import Types

import Run
import Lam.Parser

main :: IO ()
main = do
  [file,out] <- getArgs
  f <- readFile file
  writeFile "./test.wat" $ pprint $ lam2wat' $ parseLam' f
