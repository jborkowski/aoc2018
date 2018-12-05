module Day5
  ( part1
  , part2 ) where

import Data.Char (toUpper)

part1 :: String -> Int
part1 = length . foldr go ""
  where
    go h (x : xs) | h /= x && toUpper h == toUpper x = xs
    go h xs = h : xs

part2 :: String -> Int
part2 input = minimum polimersLen
  where
    isGood wrong x = x /= wrong && x /= toUpper wrong 
    polimersLen = [part1 (filter (isGood wrong) input) | wrong <- ['a'..'z']]
