{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}

module Run where

import Development.Shake

import Val
import Commands
import Union
import Types hiding (Fix)
import qualified Types as T (Fix)

import Lam.Parser

import Lam.Syntax
import Cps.Syntax
import Wat.Syntax

import Tree.Syntax
import Tps.Syntax

import Trans.Lam2Cps
import Trans.Cps2Cps
import Trans.Cps2Wat

import Trans.Lam2Tree
import Trans.Tree2Tps
import Trans.Tps2Tps
import Trans.Tps2Wat

compile :: FilePath -> FilePath -> IO ()
compile file outFile = do
  fileContents <- readFile file
  case parseLam fileContents of
    Left err -> print err
    Right exp -> writeFile outFile $ pprint $ lam2wat' exp


runWatFile :: FilePath -> FilePath -> IO ()
runWatFile watfile wasmfile = do
  cmd_ wat2wasm [watfile, "--output=" ++ wasmfile]
  cmd_ wasminterp [wasmfile, "--run-all-exports"]
  where wat2wasm = "/Users/ben/wabt/bin/wat2wasm"
        wasminterp = "/Users/ben/wabt/bin/wasm-interp"

lam2wat :: Lam -> Wat
lam2wat = cps2wat . cps2cps. lam2cps

lam2wat' :: Lam -> Wat
lam2wat' = tps2wat . tps2tps . tree2tps . lam2tree
