{-# Language OverloadedStrings #-}
module Day3
  (
  ) where

-- A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge,
-- 2 inches from the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of
-- fabric represented by # (and ignores the square inches of fabric represented by .) in the diagram below

data Claim = Claim { id, xOffset, yOffset, xSize, ySize :: !Int }

-- Parse claim description; example #1 @ 1,3: 4x4
parse :: String -> Claim
parse _ = error "Unimplemented function ‘parse’"




