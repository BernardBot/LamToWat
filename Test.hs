module Test where

import Lam
import Wat

import Cps
import Lam2Cps
import Cps2Cps
import Cps2Wat

import Tree
import Lam2Tree
import Tree2Tree
import Tree2Wat

testAll str = do
  let lam = str2lam str
  let cps = lam2cps $ str2lam $ str
  let cps' = cps2cps $ lam2cps $ str2lam $ str
  let wat = cps2wat $ cps2cps $ lam2cps $ str2lam $ str
  let tree = tree2wat $ hFun $ hRecord $ hClosure [] $ hFresh 0 [] $ hBlock [] $ lam2tree $ str2lam str

  -- should always have integer results
  let Lam.INT lamdom = lam2dom lam
  let Cps.Int cpsdom = cps2dom cps
  let Cps.Int cpsdom' = cps2dom cps'
  let watdom = wat2dom wat
  let treedom = wat2dom tree

  let doms = [lamdom,cpsdom,cpsdom',watdom,treedom]

  putStr $ unlines $ zipWith (++)
    ["lambda", "cps", "closed cps", "wat", "wat from tree"] $
    map ((": "++) . show) doms

  putStrLn $ "TESTS " ++ if all (==lamdom) doms then "PASSED" else "FAILED"

runTest = mapM_ (\ f -> do
                    putStrLn ""
                    putStrLn f
                    file <- readFile ("test/" ++ f ++ ".lam")
                    testAll file)
          ["control", "patternmatch", "recursion", "arith"]
