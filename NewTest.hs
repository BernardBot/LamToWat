module Test where

import System.Directory

import Lam
import Wat

import Cps
import Lam2Cps
import Cps2Cps
import Cps2Wat

import Lam2CpsM
import CpsM2CpsS
import CpsS2CpsS
import CpsS2Wat

testDir :: FilePath
testDir = "./test/"

runTest :: IO ()
runTest = do
  tests <- listDirectory testDir
  mapM_ (\ f -> do
            putStrLn ""
            putStrLn f
            file <- readFile $ testDir ++ f
            testAll file)
    tests

testAll :: String -> IO ()
testAll str = do
  let lam = str2lam str
  let cps = lam2cps $ str2lam $ str
  let cps' = cps2cps $ lam2cps $ str2lam $ str
  let wat = cps2wat $ cps2cps $ lam2cps $ str2lam $ str

  let wat' = cpss2wat $ cpss2cpss $ bar $ lam2cpsm $ str2lam $ str

  -- should always have integer results
  let Lam.INT lamdom = lam2dom lam
  let Cps.Int cpsdom = cps2dom cps
  let Cps.Int cpsdom' = cps2dom cps'
  let watdom = wat2dom wat
  let watdom' = wat2dom wat'


  let doms = [lamdom,cpsdom,cpsdom',watdom,watdom']

  putStr $ unlines $ zipWith (++)
    ["lambda", "cps", "closed cps", "wat", "wat new"] $
    map ((": "++) . show) doms

  putStrLn $ "TESTS " ++ if all (==lamdom) doms then "PASSED" else "FAILED"

