module Helpers where

import System.IO
-- import Text.Megaparsec ( ParseErrorBundle, parse, errorBundlePretty ) -- megaparsec 7.0
import Text.Megaparsec ( ParseError, Parsec, parse, parseErrorPretty ) -- megaparsec 6.5
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Void
import           Data.Map (Map)
import qualified Data.Map as Map

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

type OccurrencesMap a = Map a Int
type Parser = Parsec Void String  

parseLine :: String -> Parser a -> IO a
parseLine inputLine p =
     case parse p "line.txt" inputLine of
       Left e -> fail (parseErrorPretty e)
       Right a -> return a

getParsedLines :: FilePath -> Parser a -> IO [a]
getParsedLines path p =
  do inputLines <- readLines path
     sequence $ map (\s -> parseLine s p) inputLines

parseNumber :: Integral a => Parser a
parseNumber = signed (return ()) decimal

countOccurrences :: (Ord a) => [a] -> OccurrencesMap a
countOccurrences items = Map.fromListWith (+) [ (item, 1)| item <- items]

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count predicate = foldl (\acc x -> if predicate x then acc + 1 else acc) 0

aggregateOccurrences :: (Ord a) => (Num b) => [(a, b)] -> Map a b
aggregateOccurrences = Map.fromListWith (+)
