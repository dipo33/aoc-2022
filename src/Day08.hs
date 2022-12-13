module Main where

import Data.Bool.HT (if')
import Data.Char (digitToInt)
import Data.List (nub)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Parsec (digit, endBy1, many1, spaces)
import Text.Parsec.String (Parser)
import qualified Utils

type TreeGrid = Matrix Int

type TreePos = (Int, Int)

type VisibleTrees = Set TreePos

main :: IO ()
main = do
  trees <- Utils.parseFile grid "day08"
  Utils.firstResult . length . visible $ trees
  Utils.secondResult . maximum . Matrix.mapPos (scenic trees) $ trees

directions :: TreeGrid -> [[(TreePos, Int)]]
directions trees =
  let trees' = Matrix.mapPos (,) trees
      rows = Matrix.toLists trees'
      cols = Matrix.toLists . Matrix.transpose $ trees'
   in concat [rows, cols, map reverse rows, map reverse cols]

visible :: TreeGrid -> [TreePos]
visible trees =
  let dirs = directions trees
   in nub . concatMap (fst . foldr visFold ([], -1)) $ dirs
  where
    visFold :: (TreePos, Int) -> ([TreePos], Int) -> ([TreePos], Int)
    visFold (pos, x) (vis, max') = if' (x > max') (pos : vis, x) (vis, max')

directionsFrom :: TreePos -> TreeGrid -> [Vector Int]
directionsFrom (y, x) trees =
  let right = Vector.drop x . Matrix.getRow y $ trees
      down = Vector.drop y . Matrix.getCol x $ trees
      left = Vector.reverse . Vector.take (x - 1) . Matrix.getRow y $ trees
      up = Vector.reverse . Vector.take (y - 1) . Matrix.getCol x $ trees
   in [right, down, left, up]

scenic :: TreeGrid -> TreePos -> Int -> Int
scenic trees pos val = product . map directionScore . directionsFrom pos $ trees
  where
    directionScore :: Vector Int -> Int
    directionScore vec
      | Vector.null vec = 0
      | Vector.head vec < val = (+ 1) . directionScore . Vector.tail $ vec
      | otherwise = 1

-- Parsers --
grid :: Parser TreeGrid
grid = Matrix.fromLists <$> endBy1 (many1 $ digitToInt <$> digit) spaces
