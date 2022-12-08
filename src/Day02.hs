{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (liftA2, (<|>))
import Data.Bool.HT (if')
import Data.Tuple.Extra ((&&&))
import Text.Parsec (char, endBy1, oneOf, spaces)
import Text.Parsec.String (Parser)
import qualified Utils

data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

data Outcome = Win | Loss | Draw
  deriving (Show)

instance Ord Shape where
  compare :: Shape -> Shape -> Ordering
  Rock `compare` Scissors = GT
  Paper `compare` Rock = GT
  Scissors `compare` Paper = GT
  x `compare` y = if' (x == y) EQ LT

main :: IO ()
main = do
  strat <- Utils.parseFile strategy "day02"
  properStrat <- Utils.parseFile properStrategy "day02"
  Utils.firstResult . sum . map (uncurry totalPoints) $ strat
  Utils.secondResult . sum . map (uncurry totalPoints . assemble) $ properStrat
  where
    assemble = fst &&& uncurry guessShape

shapes :: [Shape]
shapes = [Rock, Paper, Scissors]

guessShape :: Shape -> Outcome -> Shape
guessShape x Draw = x
guessShape x Win = head . filter (> x) $ shapes
guessShape x Loss = head . filter (< x) $ shapes

outcomePoints :: Shape -> Shape -> Int
outcomePoints enemy me = case compare me enemy of
  GT -> 6
  EQ -> 3
  LT -> 0

shapePoints :: Shape -> Int
shapePoints Rock = 1
shapePoints Paper = 2
shapePoints Scissors = 3

totalPoints :: Shape -> Shape -> Int
totalPoints enemy me = outcomePoints enemy me + shapePoints me

-- Parser --
shape :: Parser Shape
shape = (Rock <$ oneOf "AX") <|> (Paper <$ oneOf "BY") <|> (Scissors <$ oneOf "CZ")

outcome :: Parser Outcome
outcome = (Loss <$ char 'X') <|> (Draw <$ char 'Y') <|> (Win <$ char 'Z')

strategy :: Parser [(Shape, Shape)]
strategy = endBy1 shapePair spaces
  where
    shapePair = liftA2 (,) (shape <* spaces) shape

properStrategy :: Parser [(Shape, Outcome)]
properStrategy = endBy1 strategyRound spaces
  where
    strategyRound = liftA2 (,) (shape <* spaces) outcome
