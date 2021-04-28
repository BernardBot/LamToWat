module Test (tests) where

import Control.Monad

import Distribution.TestSuite

import System.Directory

import Interpreter (run0)
import Run (lam2doms,lam2doms')

import Lam (parseLam)

testDir :: FilePath
testDir = "./test"

testfileDir :: FilePath
testfileDir = testDir ++ "/lam/"

testfilenames :: IO [FilePath]
testfilenames = listWithDirectory testfileDir

testfiles :: IO [String]
testfiles = testfilenames >>= mapM readFile

testResults :: IO [Progress]
testResults = do
  ns <- testfilenames
  fs <- testfiles
  zipWithM
    (\ n f -> do
        putStrLn ""
        putStrLn n
        testIt f)
    ns fs

tests :: IO [Test]
tests = do
  filenames <- testfilenames
  files <- testfiles
  return $ zipWith
    (\ filename file -> Test $ TestInstance
       { run = testIt file
       , name = filename
       , tags = []
       , options = []
       , setOption = undefined
       })
    filenames
    files

testIt :: String -> IO Progress
testIt str = case parseLam str of
  Left err -> return $ Finished $ Fail $ show err
  Right exp -> do
    let r  = lam2doms exp
    let r' = lam2doms' exp
    let rs = r ++ r'
    print r
    print r'
    if all (==head r) rs
      then do
        putStrLn "TEST PASSED"
        return $ Finished Pass
       else do
        putStrLn "TEST FAILED"
        return $ Finished $ Fail $ show r ++ " " ++ show r'

listWithDirectory :: FilePath -> IO [FilePath]
listWithDirectory dir = fmap (fmap (dir++)) (listDirectory dir)
