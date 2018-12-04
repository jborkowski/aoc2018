module Main where

import Day1
import Day2

import Helpers

main :: IO ()
main = do
  content1 <- readLines "inputs/Day1.txt"
  content2 <- readLines "inputs/Day2.txt"
  print (Day1.part1 content1)
  print (Day1.part2 content1)
  print (Day2.part1 content2)
  print (Day2.part2 content2)
