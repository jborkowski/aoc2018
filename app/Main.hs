module Main where

import Day1
import Day2

import System.IO

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main :: IO ()
main = do
  res11    <- Day1.part1 "inputs/Day1.txt"
  res12    <- Day1.part2 "inputs/Day1.txt"
  content2 <- readLines "inputs/Day2.txt"
  print res11
  print res12
  print (Day2.part1 content2)
  print (Day2.part2 content2)
