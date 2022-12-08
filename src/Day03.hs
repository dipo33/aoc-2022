module Main where

import Data.Char (isLower, ord)
import Data.List (intersect)
import Text.Parsec (endBy1, letter, many1, spaces)
import Text.Parsec.String (Parser)
import qualified Utils
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  sacks <- Utils.parseFile rucksacks "day03"
  Utils.firstResult . sum . map (priority . head . uncurry intersect) $ sacks
  Utils.secondResult . sum . map (priority . head . foldr1 intersect) . chunksOf 3 . map (uncurry (++)) $ sacks

priority :: Char -> Int
priority c
  | isLower c = ord c - 97 + 1
  | otherwise = ord c - 65 + 27

-- Parsers --
rucksacks :: Parser [(String, String)]
rucksacks = endBy1 rucksack spaces

rucksack :: Parser (String, String)
rucksack = split <$> many1 letter
  where
    half :: String -> Int
    half = (`div` 2) . length

    split :: String -> (String, String)
    split x = splitAt (half x) x
