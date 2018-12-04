module Helpers
  ( readLines
  ) where

import System.IO
import System.Environment
import Text.Printf
import Text.Megaparsec (many, parse, parseErrorTextPretty, Parsec, eof)
import Text.Megaparsec.Error (errorBundlePretty)
import Data.Void
import Data.List
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

type Parser = Parsec Void String  

parseLine :: IO a -> Parser a -> IO a
parseLine inputLine p =
  do line <- inputLine
     case parse p "line.txt" line of
       Left e -> fail (errorBundlePretty e)
       Right a -> return a

getParsedLines :: FilePath -> Parser a -> IO [a]
getParsedLines path p = error "Z"

  

-- getParsedLines :: Int -> Parser a -> IO [a]
-- getParsedLines i p = getParsedInput i (many (p <* newline) <* eof)
  
