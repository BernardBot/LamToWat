module Main where

import Run

import System.Environment

main :: IO ()
main = do
  [file,out] <- getArgs
  f <- readFile file
  str2watfile f out
