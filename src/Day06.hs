module Main where

import Data.List (findIndex, nub, tails)
import Data.Maybe (fromJust)
import Text.Parsec (letter, many1)
import Text.Parsec.String (Parser)
import qualified Utils

main :: IO ()
main = do
  stream <- Utils.parseFile datastream "day06"
  Utils.firstResult . (+ 4) . fromJust . findIndex ((== 4) . length . nub) . markers 4 $ stream
  Utils.secondResult . (+ 14) . fromJust . findIndex ((== 14) . length . nub) . markers 14 $ stream

markers :: Int -> String -> [String]
markers size = filter ((== size) . length) . map (take size) . tails

-- Parsers --
datastream :: Parser String
datastream = many1 letter
