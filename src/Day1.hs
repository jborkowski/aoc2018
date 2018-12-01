module Day1
  ( calcResult
  ) where

import System.IO

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeInt :: [String] -> [Int]
makeInt =  map toInt

toInt :: String -> Int
toInt = read . filter (/= '+')  

calcResult :: FilePath -> IO Int
calcResult path = fmap (sum . makeInt) (readLines path)
