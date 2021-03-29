module Main where

import Development.Shake

import System.Directory

import Lam
import Wat

import Cps
import Lam2Cps
import Cps2Cps
import Cps2Wat

import Lam2Tree
import Tree2Tps
import Tps2Tps
import Tps2Wat

main :: IO ()
main = print "Hello World!"

str2cps :: String -> Cps
str2cps = lam2cps . str2lam

str2ccps :: String -> Cps
str2ccps = cps2cps . lam2cps . str2lam

str2cwat :: String -> Wat
str2cwat = cps2wat . cps2cps . lam2cps . str2lam

str2twat :: String -> Wat
str2twat = tps2wat . hFun . hRecord . hClos . tree2tps . lam2tree . str2lam

str2watfile :: String -> FilePath -> IO ()
str2watfile str watfile = writeFile watfile $ show $ str2cwat str

watfile2res :: FilePath -> FilePath -> IO ()
watfile2res watfile wasmfile = do
  cmd_ wat2wasm [watfile, "--output=" ++ wasmfile]
  cmd_ wasminterp [wasmfile, "--run-all-exports"]
  where wat2wasm = "/Users/ben/wabt/bin/wat2wasm"
        wasminterp = "/Users/ben/wabt/bin/wasm-interp"
