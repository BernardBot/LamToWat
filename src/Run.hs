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
import Interpreter
import Types hiding (Fix)
import qualified Types as T (Fix)

import Lam.Parser

import Lam.Syntax
import Cps.Syntax
import Hps.Syntax
import Wat.Syntax

import Tree.Syntax
import Tps.Syntax

import Trans.Lam2Cps
import Trans.Cps2Hps
import Trans.Hps2Wat

import Trans.Lam2Tree hiding (LamTree)
import Trans.Tree2Tps
import Trans.Tps2Tps
import Trans.Tps2Wat

runWatFile :: FilePath -> FilePath -> IO ()
runWatFile watfile wasmfile = do
  cmd_ wat2wasm [watfile, "--output=" ++ wasmfile]
  cmd_ wasminterp [wasmfile, "--run-all-exports"]
  where wat2wasm = "/Users/ben/wabt/bin/wat2wasm"
        wasminterp = "/Users/ben/wabt/bin/wasm-interp"

runCps :: String -> EDom
runCps =
  run0 .
  (transform :: Hps -> Wat) .
  (transform :: Cps -> Hps) .
  (transform :: Lam -> Cps) .
  parseLam'

runTps :: String -> EDom
runTps =
  run0 .
  (transform :: T.Fix (Tps (Malloc :+: Base :+: Empty) Val) -> Wat) .
  (transform :: Tps (Fix :+: Malloc :+: Base :+: Empty) Val -> T.Fix (Tps (Malloc :+: Base :+: Empty) Val)) .
  (transform :: Tps (Malloc :+: Fix :+: Base :+: Empty) Val -> Tps (Fix :+: Malloc :+: Base :+: Empty) Val) .
  (transform :: Tps (Record :+: Fix :+: Base :+: Empty) Val -> Tps (Malloc :+: Fix :+: Base :+: Empty) Val) .
  (transform :: Tps (Fix :+: Base :+: Empty)            Val -> Tps (Record :+: Fix :+: Base :+: Empty) Val) .
  (transform :: Tree (Comp :+: Fix :+: Base)            Val -> Tps (Fix :+: Base :+: Empty) Val) .
  (transform :: Lam                                         -> Tree (Comp :+: Fix :+: Base) Val) .
  parseLam'
