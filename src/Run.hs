module Run where

import Text.PrettyPrint.HughesPJ
import Development.Shake
import Data.Tree.Pretty
import Text.Show.Pretty

import Types

import Interpreter

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

parse :: String -> Lam
parse = str2lam

compile :: FilePath -> FilePath -> IO ()
compile file outFile = do
  fileContents <- readFile file
  case parseLam fileContents of
    Left err -> print err
    Right exp -> writeFile outFile $ emit $ lam2wat' exp

lam2wat :: Lam -> Wat
lam2wat = cps2wat . cps2cps. lam2cps

lam2wat' :: Lam -> Wat
lam2wat' = tps2wat . tps2tps . tree2tps . lam2tree

lam2doms :: Lam -> [Either Error Dom]
lam2doms lam = [run0 lam, run0 lam',run0 lam'',run0 lam''']
  where lam' = lam2cps lam
        lam'' = cps2cps lam'
        lam''' = cps2wat lam''

lam2doms' :: Lam -> [Either Error Dom]
lam2doms' lam = [run0 lam, run0 lam',run0 lam'',run0 lam''',run0 lam'''',run0 lam''''']
  where lam' = tree2tps $ lam2tree lam
        lam'' = hClos lam'
        lam''' = hRecord lam''
        lam'''' = hFix $ swapTps lam'''
        lam''''' = tps2wat lam''''
