module Main where

import AdventParser (integer)
import Control.Applicative (liftA2)
import Text.Parsec (char, endBy1, spaces)
import Text.Parsec.String (Parser)
import qualified Utils

type Assignment = (Int, Int)

main :: IO ()
main = do
  assignments <- Utils.parseFile assignmentPairs "day04"
  Utils.firstResult . length . filter (uncurry contains) $ assignments
  Utils.secondResult . length . filter (uncurry overlaps) $ assignments

contains :: Assignment -> Assignment -> Bool
contains a b = containsMS a b || containsMS b a
  where
    containsMS (m1, m2) (s1, s2) = m1 <= s1 && m2 >= s2

overlaps :: Assignment -> Assignment -> Bool
overlaps (a1, a2) (b1, b2) = a1 <= b2 && b1 <= a2

-- Parsers --
assignment :: Parser Assignment
assignment = liftA2 (,) (integer <* char '-') integer

assignmentPair :: Parser (Assignment, Assignment)
assignmentPair = liftA2 (,) (assignment <* char ',') assignment

assignmentPairs :: Parser [(Assignment, Assignment)]
assignmentPairs = endBy1 assignmentPair spaces
