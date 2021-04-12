{-# LANGUAGE TypeOperators #-}
module Run where

import Development.Shake

import System.Directory

import Val
import Lam
import Wat

import Cps
import Lam2Cps
import Cps2Cps
import Cps2Wat

import Tps
import Tree hiding ((:+:),Fix,Base)
import Lam2Tree
import Tree2Tps
import Tps2Tps
import Tps2Wat

printAll :: [String -> IO ()] -> String -> IO ()
printAll fs str = mapM_ (\ f -> f str >> putStrLn "" >> putStrLn "") fs

---------
-- Cps --
---------

str2cps :: String -> Cps
str2cps = lam2cps . str2lam

str2ccps :: String -> Cps
str2ccps = cps2cps . lam2cps . str2lam

str2cwat :: String -> Wat
str2cwat = cps2wat . cps2cps . lam2cps . str2lam

fcps :: [String -> IO ()]
fcps = [ print . str2lam
       , print . str2cps
       , print . str2ccps
       , print . str2cwat
       ]

printAllCps :: String -> IO ()
printAllCps = printAll fcps

---------
-- Tps --
---------

str2tree :: String -> Tree Cmd Val
str2tree = lam2tree . str2lam

str2tps :: String -> Tps (Fix :+: Base :+: VoidCmd) Val
str2tps = tree2tps . lam2tree . str2lam

str2tps' :: String -> Tps (Record :+: Fix :+: Base :+: VoidCmd) Val
str2tps' = hClos . tree2tps . lam2tree . str2lam

str2tps'' :: String -> Tps (Malloc :+: Fix :+: Base :+: VoidCmd) Val
str2tps'' = hRecord . hClos . tree2tps . lam2tree . str2lam

str2tps''' :: String -> ([(String,[String],TpsWat)],TpsWat)
str2tps''' = hFix . swap . hRecord . hClos . tree2tps . lam2tree . str2lam

str2twat :: String -> Wat
str2twat = tps2wat . hFix . swap . hRecord . hClos . tree2tps . lam2tree . str2lam

ftps :: [String -> IO ()]
ftps = [ print . str2lam
       , print . str2tree
       , print . str2tps
       , print . str2tps'
       , print . str2tps''
       , print . str2tps'''
       , print . str2twat
       ]

printAllTps :: String -> IO ()
printAllTps = printAll ftps

-----------------
-- WebAssembly --
-----------------

str2watfile :: String -> FilePath -> IO ()
str2watfile str watfile = writeFile watfile $ show $ str2cwat str

watfile2res :: FilePath -> FilePath -> IO ()
watfile2res watfile wasmfile = do
  cmd_ wat2wasm [watfile, "--output=" ++ wasmfile]
  cmd_ wasminterp [wasmfile, "--run-all-exports"]
  where wat2wasm = "/Users/ben/wabt/bin/wat2wasm"
        wasminterp = "/Users/ben/wabt/bin/wasm-interp"

------------------
-- Interpreting --
------------------

str2lam2int :: String -> Int
str2lam2int = lam2int . str2lam

str2cps2int :: String -> Int
str2cps2int = cps2int . str2cps

str2ccps2int :: String -> Int
str2ccps2int = cps2int . str2ccps

str2cwat2int :: String -> Int
str2cwat2int = wat2int . str2cwat

str2twat2int :: String -> Int
str2twat2int = wat2int . str2twat
