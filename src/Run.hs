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

import Lam.Syntax
import Cps.Syntax
import Hps.Syntax
import Wat.Syntax

import Tree.Syntax
import Tps.Syntax

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

lam2wat :: Lam -> Wat
lam2wat =
  (transform :: Hps -> Wat) .
  (transform :: Cps -> Hps) .
  (transform :: Lam -> Cps)

lam2wat' :: Lam -> Wat
lam2wat' =
  (transform :: T.Fix (Tps (Malloc :+: Base :+: Empty)  Val) -> Wat) .
  (transform :: Tps (Fix :+: Malloc :+: Base :+: Empty) Val  -> T.Fix (Tps (Malloc :+: Base :+: Empty)  Val)) .
  (transform :: Tps (Malloc :+: Fix :+: Base :+: Empty) Val  -> Tps (Fix :+: Malloc :+: Base :+: Empty) Val) .
  (transform :: Tps (Record :+: Fix :+: Base :+: Empty) Val  -> Tps (Malloc :+: Fix :+: Base :+: Empty) Val) .
  (transform :: Tps (Fix :+: Base :+: Empty)            Val  -> Tps (Record :+: Fix :+: Base :+: Empty) Val) .
  (transform :: Tree (Comp :+: Fix :+: Base)            Val  -> Tps (Fix :+: Base :+: Empty)            Val) .
  (transform :: Lam                                          -> Tree (Comp :+: Fix :+: Base)            Val)
