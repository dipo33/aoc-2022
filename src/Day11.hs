{-# LANGUAGE InstanceSigs #-}

module Main where

import AdventParser (integer)
import Control.Applicative (liftA2, liftA3, (<|>))
import Control.Lens (Lens', (%~))
import Control.Lens.Lens (lens)
import Data.Bool.HT (if')
import Data.Functor (($>))
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (char, endBy1, sepBy, spaces, string)
import Text.Parsec.String (Parser)
import qualified Utils

-- Data --
data Operand = Old | Constant Int

data Monkey' a = Monkey
  { monkeyItems :: [Item],
    monkeyWorry :: Item -> Item,
    monkeyDecide :: Item -> MonkeyId,
    monkeyDivisor :: Int,
    monkeyData :: a
  }

createMonkey :: [Item] -> (Item -> Item) -> (Item -> MonkeyId) -> Int -> Monkey
createMonkey items worry decide divisor = Monkey items worry decide divisor 0

instance (Show a) => Show (Monkey' a) where
  show :: Monkey' a -> String
  show (Monkey items _ _ _ bus) = "Monkey (Items: " ++ show items ++ ", Bussiness: " ++ show bus ++ ")"

type Monkey = Monkey' MonkeyData

type Monkeys = Map MonkeyId Monkey

type Item = Int

type MonkeyId = Int

type MonkeyData = Int

monkeyItemsL :: Lens' (Monkey' a) [Item]
monkeyItemsL = lens monkeyItems (\o v -> o {monkeyItems = v})

-- Solution --
main :: IO ()
main = do
  monkeys <- Utils.parseFile monkeyBlocks "day11"
  Utils.firstResult . product . take 2 . reverse . sort . map monkeyData . Map.elems . simulateRounds False 20 $ monkeys
  Utils.secondResult . product . take 2 . reverse . sort . map monkeyData . Map.elems . simulateRounds True 10000 $ monkeys

throwTo :: Item -> MonkeyId -> Monkeys -> Monkeys
throwTo item = Map.adjust addItem
  where
    addItem :: Monkey -> Monkey
    addItem = monkeyItemsL %~ (++ [item])

monkeyTurn :: Bool -> MonkeyId -> Monkeys -> Monkeys
monkeyTurn distressed mId monkeys =
  let (Monkey items worry decide divisor bus) = monkeys Map.! mId
      items' = map (relieve . worry) items
      targets = map decide items'
      monkeys' = Map.insert mId (Monkey [] worry decide divisor (bus + length items)) monkeys
   in foldl (\monks (item, target) -> throwTo item target monks) monkeys' $ zip items' targets
  where
    relieve :: Item -> Item
    relieve x = if' distressed x (x `div` 3)

simulateRounds :: Bool -> Int -> Monkeys -> Monkeys
simulateRounds _ 0 m = m
simulateRounds distressed c m = simulateRounds distressed (c - 1) $ monkeyRound distressed m

monkeyRound :: Bool -> Monkeys -> Monkeys
monkeyRound distressed monkeys =
  let keys = Map.keys monkeys
   in optimizeWorries $ foldl (flip . monkeyTurn $ distressed) monkeys keys
  where
    optimizeWorries :: Monkeys -> Monkeys
    optimizeWorries monkeys' =
      let totalLcm = foldr lcm 1 $ Map.map monkeyDivisor monkeys'
       in Map.map (monkeyItemsL %~ map (`mod` totalLcm)) monkeys'

-- Parsers --
monkeyBlocks :: Parser Monkeys
monkeyBlocks = Map.fromAscList <$> endBy1 monkeyBlock spaces

monkeyBlock :: Parser (Int, Monkey)
monkeyBlock = do
  mId <- monkeyId
  startingItems <- startingItemsLine
  operation <- operationLine
  (decider, divisor) <- testBlock
  return (mId, Monkey startingItems operation decider divisor 0)
  where
    monkeyId :: Parser Int
    monkeyId = string "Monkey " *> integer <* char ':'

entryLine :: String -> Parser a -> Parser a
entryLine name parser = spaces *> string name *> string ": " *> parser

startingItemsLine :: Parser [Int]
startingItemsLine = entryLine "Starting items" $ sepBy integer (string ", ")

operationLine :: Parser (Int -> Int)
operationLine = entryLine "Operation" $ string "new = " *> operation
  where
    operation :: Parser (Int -> Int)
    operation = liftA3 constructOperation (operand <* char ' ') operator (char ' ' *> operand)

    operator :: Parser (Int -> Int -> Int)
    operator = (char '*' $> (*)) <|> (char '+' $> (+))

    operand :: Parser Operand
    operand = (string "old" $> Old) <|> (Constant <$> integer)

    constructOperation :: Operand -> (Int -> Int -> Int) -> Operand -> (Int -> Int)
    constructOperation Old op Old = \x -> op x x
    constructOperation Old op (Constant x) = op x
    constructOperation (Constant x) op Old = op x
    constructOperation (Constant x) op (Constant y) = \_ -> op x y

testLine :: Parser (Int -> Bool)
testLine = constructDivisibility <$> entryLine "Test" (string "divisible by " *> integer)
  where
    constructDivisibility :: Int -> Int -> Bool
    constructDivisibility x y = (y `mod` x) == 0

testBlock :: Parser (Item -> MonkeyId, Int)
testBlock = liftA2 (\divisor decider -> (decider . (== 0) . (`mod` divisor), divisor)) divisibleLine boolLines
  where
    boolLines :: Parser (Bool -> MonkeyId)
    boolLines = liftA2 constructCheck (boolLine "true") (boolLine "false")

    boolLine :: String -> Parser MonkeyId
    boolLine bool = entryLine ("If " ++ bool) $ string "throw to monkey " *> integer

    divisibleLine :: Parser Int
    divisibleLine = entryLine "Test" (string "divisible by " *> integer)

    constructCheck :: MonkeyId -> MonkeyId -> Bool -> MonkeyId
    constructCheck monkeyId _ True = monkeyId
    constructCheck _ monkeyId False = monkeyId
