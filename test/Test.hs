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
main = do
  tests <- listDirectory testDir
  mapM_ (\ f -> do
            putStrLn ""
            putStrLn f
            file <- readFile $ testDir ++ f
            testAll file)
    tests

testDir :: FilePath
testDir = "./test/lam/"

testAll :: String -> IO ()
testAll str = do
  let lam = str2lam str
  let cps = lam2cps $ str2lam $ str
  let cps' = cps2cps $ lam2cps $ str2lam $ str
  let wat = cps2wat $ cps2cps $ lam2cps $ str2lam $ str
  let wat' = tps2wat $ hFun $ hRecord $ hClos $ tree2tps $ lam2tree $ str2lam $ str

  -- should always have integer results
  let Lam.INT lamdom = lam2dom lam
  let Cps.Int cpsdom = cps2dom cps
  let Cps.Int cpsdom' = cps2dom cps'
  let watdom = wat2dom wat
  let watdom' = wat2dom wat'

  str2res str

  let doms = [lamdom,cpsdom,cpsdom',watdom,watdom']

  putStr $ unlines $ zipWith (++)
    ["lambda", "cps", "closed cps", "wat", "wat from tree"] $
    map ((": "++) . show) doms

  putStrLn $ "TESTS " ++ if all (==lamdom) doms then "PASSED" else error "FAILED"

str2wat = cps2wat . cps2cps . lam2cps . str2lam
str2watfile str watfile = writeFile watfile $ show $ str2wat str

testfile = "test"
testwatfile = "./test/" ++ testfile ++ ".wat"
testwasmfile = "./test/" ++ testfile ++ ".wasm"
wat2wasm = "/Users/ben/wabt/bin/wat2wasm"
wasminterp = "/Users/ben/wabt/bin/wasm-interp"

str2res :: String -> IO ()
str2res str = do
  str2watfile str testwatfile
  cmd_ wat2wasm [testwatfile, "--output=" ++ testwasmfile]
  cmd_ wasminterp [testwasmfile, "--run-all-exports"]
