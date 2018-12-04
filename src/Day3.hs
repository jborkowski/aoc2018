{-# Language OverloadedStrings #-}
module Day3
  ( part1
  , part2
  , claimParser
  , costiumFabric
  ) where

import           Helpers
import           Data.Map (Map)
import qualified Data.Map as Map


-- A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge,
-- 2 inches from the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of
-- fabric represented by # (and ignores the square inches of fabric represented by .) in the diagram below

data Claim = Claim { claimId, xOffset, yOffset, xSize, ySize :: !Int } deriving (Show, Read)

-- Parse claim description; example #1 @ 1,3: 4x4
claimParser :: Parser Claim
claimParser = Claim <$ "#"    <*> parseNumber
                    <* " @ "  <*> parseNumber
                    <* ","    <*> parseNumber
                    <* ": "   <*> parseNumber
                    <* "x"    <*> parseNumber
                  

claimCoord :: Claim -> [(Int, Int)]
claimCoord claim =
  [ (x, y)
  | x <- [xOffset claim .. xOffset claim + xSize claim - 1]
  , y <- [yOffset claim .. yOffset claim + ySize claim - 1]
  ]

costiumFabric :: [Claim] -> OccurrencesMap (Int, Int)
costiumFabric = countOccurrences . concatMap claimCoord

part1 :: OccurrencesMap (Int, Int) -> Int
part1 = count (> 1)

part2 :: OccurrencesMap (Int, Int) -> [Claim] -> Int
part2 fabric claims =
  head [ claimId claim
       | claim <- claims
       , all (1 ==) (Map.intersection fabric ( costiumFabric [claim]) ) 
       ]

  



