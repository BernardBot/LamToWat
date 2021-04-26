module Run where

import Development.Shake

import Types

import Lam
import Wat

import Cont.Cps

import CTree.Tree
import CTree.Tps

import Cont.Lam2Cps
import Cont.Cps2Cps
import Cont.Cps2Wat

import CTree.Lam2Tree
import CTree.Tree2Tps
import CTree.Tps2Tps
import CTree.Tps2Wat

compile :: FilePath -> FilePath -> IO ()
compile file outFile = do
  fileContents <- readFile file
  case parseLam fileContents of
    Left err -> print err
    Right exp -> writeFile outFile $ emit $ lam2wat' exp

runWatFile :: FilePath -> FilePath -> IO ()
runWatFile watfile wasmfile = do
  wat2wasm watfile wasmfile
  wasminterp wasmfile

lam2wat :: Lam -> Wat
lam2wat = cps2wat . cps2cps. lam2cps

lam2wat' :: Lam -> Wat
lam2wat' = tps2wat . tps2tps . tree2tps . lam2tree
