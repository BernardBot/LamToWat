module Main where

import System.Environment

import Types

import Run
import Lam

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file,"run"] -> do
      compile file "./temp.wat"
      wat2wasm "./temp.wat" "./temp.wasm"
    [file,outFile,"run"] -> do
      compile file outFile
      wat2wasm outFile "./temp.wasm"

    [file]         -> compile file "./temp.wat"
    [file,outFile] -> compile file outFile

    _ -> putStrLn "use lam2wat like: lam2wat file [outputfile] [run]"
