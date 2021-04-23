{-# LANGUAGE TupleSections #-}

module Wat.Run where

import Development.Shake

import Types

import Wat.Interpreter
import Wat.Syntax

run :: Expr -> Dom
run = runInterp . interp

runExp :: Exp -> Dom
runExp = run . ([],)

runWatFile :: String -> IO ()
runWatFile watfile = do
  cmd_ wat2wasm [watfile, "--output=" ++ tmp]
  cmd_ wasminterp [tmp, "--run-all-exports"]
  where wat2wasm = "/Users/ben/wabt/bin/wat2wasm"
        wasminterp = "/Users/ben/wabt/bin/wasm-interp"
        tmp = "tmpwasmfile.wasm"
