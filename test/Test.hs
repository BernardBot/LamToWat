module Test (tests) where

import Distribution.TestSuite

import System.Directory

import Run

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
       { run = testAllResult file
       , name = filename
       , tags = []
       , options = []
       , setOption = undefined
       })
    testfilenames
    testfiles

testAllResult :: String -> IO Progress
testAllResult str = case testAll str of
      True  -> return (Finished Pass)
      False -> return (Finished (Fail (show (runToInts str))))

runToInts :: String -> [Int]
runToInts str = map ($str) toInts

testAll :: String -> Bool
testAll = allEq . runToInts

toInts :: [String -> Int]
toInts = [ str2lam2int
         , str2cps2int
         , str2ccps2int
         , str2cwat2int
         , str2twat2int
         ]

allEq :: Eq a => [a] -> Bool
allEq []     = True
allEq (x:xs) = go xs
  where go []     = True
        go (y:xs) = if x == y then go xs else False
        
