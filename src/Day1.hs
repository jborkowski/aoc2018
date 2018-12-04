module Day1
  ( part1
  , part2
  ) where

import Data.Maybe ( fromJust )
import qualified Data.Set as Set

makeInteger :: String -> Integer
makeInteger = read . filter (/= '+')  

part1 :: [String] -> Integer
part1 = sum . map makeInteger

partSums :: (Num n) => [n] -> [n]
partSums = scanl (+) 0

part2 :: [String] -> Integer
part2 =  fromJust . firstDuplicate . partSums . cycle . map makeInteger

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (x:xs)
      | x `Set.member` seen = Just x
      | otherwise           = go (Set.insert x seen) xs
