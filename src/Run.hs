{-# LANGUAGE TypeOperators #-}
module Run where

import Development.Shake

import Lam.Syntax
import Wat.Syntax


import Trans.Cps2CpsH
import Trans.Cps2Wat
import Trans.Lam2Tree
import Trans.Tps2Tps
import Trans.Tps2Wat
import Trans.Tree2Tps


runWatFile :: FilePath -> FilePath -> IO ()
runWatFile watfile wasmfile = do
  cmd_ wat2wasm [watfile, "--output=" ++ wasmfile]
  cmd_ wasminterp [wasmfile, "--run-all-exports"]
  where wat2wasm = "/Users/ben/wabt/bin/wat2wasm"
        wasminterp = "/Users/ben/wabt/bin/wasm-interp"

lam2wat :: Lam.Syntax.Expr -> Wat.Syntax.Expr
lam2wat = cps2cpsH . lam2cps
