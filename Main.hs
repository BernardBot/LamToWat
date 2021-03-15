module Main where

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

main :: IO ()
main = interact $ show . tree2wat . hFun . hRecord . hClosure . hFresh . hBlock . lam2tree . str2lam

