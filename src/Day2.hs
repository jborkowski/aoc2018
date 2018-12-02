module Day2
  ( part1
  , part2
  ) where

import Data.Function (on)
import Data.List (sort, tails, group)
import Data.Ord (comparing)
import Data.Semigroup
import Data.Monoid

countAppearance :: (Eq a) => (Ord a) => [a] -> [Int]
countAppearance =  map head . group . sort . map length . group . sort

exaclyTwoAndThree :: (Ord a) => (Num a) => [a] -> [a]
exaclyTwoAndThree = filter (>= 2) . filter (<= 3)

isThereExpected :: (Eq a) => (Ord a) => [a] -> [Int]
isThereExpected = exaclyTwoAndThree . countAppearance

data Counter = Counter !Int !Int deriving (Eq,Show)

toCounter :: (Eq a) => (Num a) => [a] -> Counter
toCounter [] = Counter 0 0
toCounter (2 : []) = Counter 1 0
toCounter (3 : []) = Counter 0 1
toCounter (2:3:[]) = Counter 1 1
otherwise = Counter 0 0

hash :: Counter -> Int
hash (Counter a b) = a * b

instance Monoid Counter where
  mempty = Counter 0 0

instance Semigroup Counter where
  Counter a b <> Counter c d = Counter (a + c)  (b + d)

parseToCounter :: String -> Counter
parseToCounter = toCounter . isThereExpected


part1 :: [String] -> Int
part1 xs = hash $ foldMap parseToCounter xs    
  
test :: [String]
test = [ "abcdef"
       , "bababc"
       , "abbcde"
       , "abcccd"
       , "aabcdd"
       , "abcdee"
       , "ababab"
       ]


diffByOneElement :: (Eq a) => [a] -> [a] -> Maybe [a]
diffByOneElement (x:xs) (y:ys)
  | x == y = (x :) <$> diffByOneElement xs ys
  | xs == ys = Just xs
diffByOneElement _ _ = Nothing

part2 :: (Eq a) => [[a]] -> [a]
part2 ls = head [oneDiff | x:xs <- tails ls, y <- xs, Just oneDiff <- [diffByOneElement x y]]
