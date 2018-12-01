module Main where

import Day1

main :: IO ()
main = do
  resPart1 <- part1 "inputs/Day1.txt"
  resPart2 <- part2 "inputs/Day1.txt"
  print resPart1
  print resPart2
