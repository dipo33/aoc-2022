module Main where

import AdventParser (signedInteger)
import Control.Applicative ((<|>))
import Data.Tuple.HT (thd3)
import Text.Parsec (endBy1, spaces, string)
import Text.Parsec.String (Parser)
import qualified Utils

data Instruction = NOOP | ADDX Int
  deriving (Eq, Show)

--   Context = (State, LastExecutedCycle, Tracker)
type Context = (State, Int, Tracker)

--   Tracker = TotalSignalStrength
type Tracker = Int

--   State = RegisterX
type State = Int

main :: IO ()
main = do
  instructions' <- Utils.parseFile instructions "day10"
  Utils.firstResult . thd3 . foldl execute (1, 1, 0) $ instructions'

execute :: Context -> Instruction -> Context
execute context NOOP = updateState id . track 1 $ context
execute context (ADDX x) = updateState (+ x) . track 2 $ context

updateState :: (State -> State) -> Context -> Context
updateState f (state, cycles, tracker) = (f state, cycles, tracker)

track :: Int -> Context -> Context
track cycles (s, c, t) =
  let during = [c + x | x <- [0 .. cycles - 1]]
      duringOnly = filter ((== 0) . (`mod` 40) . (`subtract` 20)) during
      signals = map (* s) duringOnly
   in (s, c + cycles, t + sum signals)

-- Parsers --
instruction :: Parser Instruction
instruction = (NOOP <$ string "noop") <|> (ADDX <$> (string "addx " *> signedInteger))

instructions :: Parser [Instruction]
instructions = endBy1 instruction spaces
