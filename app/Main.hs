module Main where

import Day1

main :: IO ()
main = do
  result <- calcResult "inputs/day1.txt"
  print result
