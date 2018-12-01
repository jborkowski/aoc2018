module Day1
  ( part1
  , part2
  ) where

import System.IO
import Data.Maybe ( fromJust )
import qualified Data.Set as Set

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toIntArray :: [String] -> [Integer]
toIntArray =  map makeInteger

readInts :: FilePath -> IO [Integer]
readInts = fmap (toIntArray . lines) . readFile

makeInteger :: String -> Integer
makeInteger = read . filter (/= '+')  

part1 :: FilePath -> IO Integer
part1 path = fmap (sum) (readInts path)

partSums :: (Num n) => [n] -> [n]
partSums = scanl (+) 0

part2 :: FilePath -> IO Integer
part2 path = fmap (fromJust . firstDuplicate . partSums . cycle ) (readInts path)

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (x:xs)
      | x `Set.member` seen = Just x
      | otherwise           = go (Set.insert x seen) xs
