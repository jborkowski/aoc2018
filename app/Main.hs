module Main where

import Day1
import Day2
import Day3
import Day5

import Helpers

main :: IO ()
main = do
  content1 <- readLines "inputs/Day1.txt"
  content2 <- readLines "inputs/Day2.txt"
  content3 <- getParsedLines "inputs/Day3.txt" claimParser
  content5 <- readLines "inputs/Day5.txt"
  print (Day1.part1 content1)
  print (Day1.part2 content1)
  print (Day2.part1 content2)
  print (Day2.part2 content2)
  let fabric = Day3.costiumFabric content3
  print (Day3.part1 $ fabric)
  print (Day3.part2 fabric content3)
  print (Day5.part1 . head $ content5)
  print (Day5.part2 . head $ content5)
