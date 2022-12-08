module Main where

import AdventParser (integer)
import Control.Applicative (liftA2, liftA3, (<|>))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Parsec (anyChar, char, endBy1, newline, sepBy1, spaces, string, try)
import Text.Parsec.String (Parser)
import qualified Utils

type Crate = Char

type Stacks = Map Int [Crate]

type Instruction = (Int, Int, Int)

type Plan = (Stacks, [Instruction])

main :: IO ()
main = do
  scheme <- Utils.parseFile plan "day05"
  Utils.firstResult . foldr1 (++) . map ((: []) . head) . Map.elems . fst . executePlan False $ scheme
  Utils.secondResult . foldr1 (++) . map ((: []) . head) . Map.elems . fst . executePlan True $ scheme

executePlan :: Bool -> Plan -> Plan
executePlan _ p@(_, []) = p
executePlan False (s, i : ix) =
  let newPlan = (executeInstruction s i, ix)
   in executePlan False newPlan
executePlan True (s, i : ix) =
  let newPlan = (executeInstruction' s i, ix)
   in executePlan True newPlan

executeInstruction :: Stacks -> Instruction -> Stacks
executeInstruction stacks (n, from, to) =
  let stack = stacks ! from
      newStacks = Map.adjust (drop n) from stacks
      takenCrates = reverse . take n $ stack
   in Map.adjust (takenCrates ++) to newStacks

executeInstruction' :: Stacks -> Instruction -> Stacks
executeInstruction' stacks (n, from, to) =
  let stack = stacks ! from
      newStacks = Map.adjust (drop n) from stacks
      takenCrates = take n stack
   in Map.adjust (takenCrates ++) to newStacks

-- Parsers --
crate :: Parser Crate
crate = char '[' *> anyChar <* char ']'

missingCrate :: Parser Crate
missingCrate = ' ' <$ try (string "   ")

anyCrate :: Parser Crate
anyCrate = crate <|> missingCrate

crateRow :: Parser [Crate]
crateRow = sepBy1 anyCrate (char ' ')

crateRows :: Parser [[Crate]]
crateRows = endBy1 crateRow newline

stackRow :: Parser [Int]
stackRow = sepBy1 (char ' ' *> integer <* char ' ') (char ' ')

initialScheme :: Parser Stacks
initialScheme = Map.fromAscList <$> liftA2 (zipWith $ flip (,)) (reorganize <$> crateRows) stackRow
  where
    reorganize :: [[Crate]] -> [[Crate]]
    reorganize ([] : _) = []
    reorganize x = (dropWhile (== ' ') . map head $ x) : (reorganize . map tail $ x)

instruction :: Parser Instruction
instruction = liftA3 (,,) (string "move " *> integer) (string " from " *> integer) (string " to " *> integer)

instructions :: Parser [Instruction]
instructions = endBy1 instruction spaces

plan :: Parser Plan
plan = liftA2 (,) (initialScheme <* spaces) instructions