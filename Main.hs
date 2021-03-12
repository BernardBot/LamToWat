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
main = getLine >>= test

str2cps = lam2cps . str2lam
str2dom = cps2dom . cps2cps . lam2cps . str2lam

str2wat = cps2wat . cps2cps . lam2cps . str2lam

test str = do
  let lam = str2lam str
  let cps = lam2cps lam
  let cps' = cps2cps cps
  let wat = cps2wat cps'
  let tree = tree2wat $ hFun $ hRecord $ hClosure [] $ hFresh 0 $ hBlock [] $ lam2tree lam

  let lamdom = lam2dom lam
  let cpsdom = cps2dom cps
  let cpsdom' = cps2dom cps'
  let watdom = wat2dom wat
  let treedom = wat2dom tree

  let doms = [show lamdom,show cpsdom,show cpsdom',show watdom,show treedom]
  putStr $ unlines $ zipWith (++) ["cpsdom ", "cpsdom ", "clsdom ", "watdom ", "treewatdom "] doms
  if all (==(show lamdom)) doms then return () else error "not all equal result"
