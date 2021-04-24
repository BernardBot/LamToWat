{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}

module Run where

import Development.Shake

import Types

import Lam.Syntax
import Cps.Syntax
import Hps.Syntax
import Wat.Syntax

import Trans.Lam2Cps
import Trans.Cps2Hps
import Trans.Hps2Wat

import Trans.Lam2Tree
import Trans.Tree2Tps
import Trans.Tps2Tps
import Trans.Tps2Wat

runWatFile :: FilePath -> FilePath -> IO ()
runWatFile watfile wasmfile = do
  cmd_ wat2wasm [watfile, "--output=" ++ wasmfile]
  cmd_ wasminterp [wasmfile, "--run-all-exports"]
  where wat2wasm = "/Users/ben/wabt/bin/wat2wasm"
        wasminterp = "/Users/ben/wabt/bin/wasm-interp"

foo :: Lam.Syntax.Expr -> Wat.Syntax.Expr
foo =
  (transform :: Hps -> Wat) .
  (transform :: Cps -> Hps) .
  (transform :: Lam -> Cps)
