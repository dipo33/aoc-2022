module Main where

import AdventParser (integer)
import Control.Applicative (liftA2, (<|>))
import Data.Functor (($>))
import Data.List (nub)
import Data.Tuple.Extra (first, second)
import Data.Tuple.HT (thd3)
import Text.Parsec (char, endBy1, spaces)
import Text.Parsec.String (Parser)
import qualified Utils

data Direction = U | D | L | R
  deriving (Eq, Show)

type Motion = (Direction, Int)

type Pos = (Int, Int)

main :: IO ()
main = do
  mots <- Utils.parseFile motions "day09"
  Utils.firstResult . length . nub . thd3 . foldl moveRope ((0, 0), (0, 0), []) $ mots
  Utils.secondResult . length . nub . snd . foldl moveLongRope (replicate 10 (0, 0), []) $ mots

moveRope :: (Pos, Pos, [Pos]) -> Motion -> (Pos, Pos, [Pos])
moveRope state (_, 0) = state
moveRope (h, t, res) (dir, step) =
  let h' = moveHead dir h
      t' = pullTail h' t
   in moveRope (h', t', t' : res) (dir, step - 1)

moveLongRope :: ([Pos], [Pos]) -> Motion -> ([Pos], [Pos])
moveLongRope state (_, 0) = state
moveLongRope (h : ts, res) (dir, step) =
  let h' = moveHead dir h
      ts' = scanl pullTail h' ts
   in moveLongRope (ts', last ts' : res) (dir, step - 1)
moveLongRope ([], _) _ = error "should not happen"

moveHead :: Direction -> Pos -> Pos
moveHead U = second (subtract 1)
moveHead D = second (+ 1)
moveHead L = first (subtract 1)
moveHead R = first (+ 1)

pullTail :: Pos -> Pos -> Pos
pullTail (hx, hy) t@(tx, ty)
  | abs (hx - tx) > 1 || abs (hy - ty) > 1 = (tx + signum (hx - tx), ty + signum (hy - ty))
  | otherwise = t

-- Parsers --
direction :: Parser Direction
direction = (char 'U' $> U) <|> (char 'D' $> D) <|> (char 'L' $> L) <|> (char 'R' $> R)

motion :: Parser Motion
motion = liftA2 (,) (direction <* char ' ') integer

motions :: Parser [Motion]
motions = endBy1 motion spaces
