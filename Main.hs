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
main = putStrLn "Hello, Haskell!"

str2cps = lam2cps . str2lam
str2dom = cps2dom . cps2cps . lam2cps . str2lam

foo = cps2wat . cps2cps . lam2cps . str2lam

bar str = do
  let lam = str2lam str
  let cps = lam2cps lam
  let cps' = cps2cps cps
  let wat = cps2wat cps'
  let lamdom = lam2dom lam
  let cpsdom = cps2dom cps
  let cpsdom' = cps2dom cps'
  let watdom = wat2dom wat
  let doms = [show lamdom,show cpsdom,show cpsdom',show watdom]
  print $ doms
  print $ all (==(show lamdom)) doms
  
baz = tree2wat . hFun 0 . hRecord . hClosure [] . hBlock [] . lam2tree . str2lam
baz' = putStrLn . Wat.pprint . baz
