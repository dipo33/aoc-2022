module Main where

import AdventParser (signedInteger)
import Control.Applicative ((<|>))
import Data.Bool.HT (if')
import Data.List.Extra (chunksOf)
import Text.Parsec (endBy1, spaces, string)
import Text.Parsec.String (Parser)
import qualified Utils

data Instruction = NOOP | ADDX Int
  deriving (Eq, Show)

--   Context a = Context State, LastExecutedCycle, AdditionalData
data Context a = Context State Int a

--   Tracker = TotalSignalStrength
type Tracker = Int

type Screen = String

--   State = RegisterX
type State = Int

main :: IO ()
main = do
  instructions' <- Utils.parseFile instructions "day10"
  Utils.firstResult . getContextData . foldl (execute track) (Context 1 1 0) $ instructions'
  mapM_ Utils.secondResult . chunksOf 40 . reverse . getContextData . foldl (execute screen) (Context 1 1 "") $ instructions'

execute :: (Int -> Context a -> Context a) -> Context a -> Instruction -> Context a
execute f context NOOP = updateState id . f 1 $ context
execute f context (ADDX x) = updateState (+ x) . f 2 $ context

updateState :: (State -> State) -> Context a -> Context a
updateState f (Context state cycles x) = Context (f state) cycles x

getContextData :: Context a -> a
getContextData (Context _ _ x) = x

track :: Int -> Context Tracker -> Context Tracker
track cycles (Context s c t) =
  let during = [c + x | x <- [0 .. cycles - 1]]
      duringOnly = filter ((== 0) . (`mod` 40) . (`subtract` 20)) during
      signals = map (* s) duringOnly
   in Context s (c + cycles) (t + sum signals)

screen :: Int -> Context Screen -> Context Screen
screen 1 (Context state c scr) =
  let isSprite = abs ((c `mod` 40) - 1 - state) <= 1
   in Context state (c + 1) (if' isSprite '#' ' ' : scr)
screen cycles context = screen (cycles - 1) (screen 1 context)

-- Parsers --
instruction :: Parser Instruction
instruction = (NOOP <$ string "noop") <|> (ADDX <$> (string "addx " *> signedInteger))

instructions :: Parser [Instruction]
instructions = endBy1 instruction spaces
