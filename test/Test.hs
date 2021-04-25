module Test (tests) where

import Distribution.TestSuite

import System.Directory

import Interpreter (run0)
import Run (lam2wat,lam2wat')

import Lam.Parser (parseLam)

testDir :: FilePath
testDir = "./test"

testfileDir :: FilePath
testfileDir = testDir ++ "/lam/"

listWithDirectory :: FilePath -> IO [FilePath]
listWithDirectory dir = fmap (fmap (dir++)) (listDirectory dir)

tests :: IO [Test]
tests = do
  testfilenames <- listWithDirectory testfileDir
  testfiles <- mapM readFile testfilenames
  return $ zipWith
    (\ filename file -> Test $ TestInstance
       { run = testIt file
       , name = filename
       , tags = []
       , options = []
       , setOption = undefined
       })
    testfilenames
    testfiles

testIt :: String -> IO Progress
testIt str = case parseLam str of
  Left err -> return $ Finished $ Fail $ show err
  Right exp ->
    let w = lam2wat exp; r = run0 w
        w' = lam2wat' exp; r' = run0 w'
    in if r == r'
       then return $ Finished Pass
       else return $ Finished $ Fail $ show r ++ " " ++ show r'
