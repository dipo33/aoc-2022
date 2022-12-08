module Main where

import AdventParser (integer)
import Data.List (sort)
import Text.Parsec (endBy1, space)
import Text.Parsec.String (Parser)
import qualified Utils

main :: IO ()
main = do
  invs <- Utils.parseFile inventories "day01"
  Utils.firstResult . maximum . map sum $ invs
  Utils.secondResult . sum . take 3 . reverse . sort . map sum $ invs

-- Parsers --
inventory :: Parser [Int]
inventory = endBy1 integer space

inventories :: Parser [[Int]]
inventories = endBy1 inventory space
