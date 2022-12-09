module Main where

import Data.Char (digitToInt)
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (digit, endBy1, many1, spaces)
import Text.Parsec.String (Parser)
import qualified Utils

type Tree = (Int, (Int, Int))

type Visibility = Map (Int, Int) Bool

main :: IO ()
main = do
  rows <- Utils.parseFile grid "day08"
  Utils.firstResult . length . filter id . Map.elems . checkVisibility . createTreeGrid $ rows

checkVisibility :: [[Tree]] -> Visibility
checkVisibility rows =
  let columns = transpose rows
      visible = generateVisibilityMap (length columns) (length rows)
   in visibilityFold (map reverse columns) . visibilityFold (map reverse rows) . visibilityFold columns . visibilityFold rows $ visible

visibilityFold :: [[Tree]] -> Visibility -> Visibility
visibilityFold [] v = v
visibilityFold (x : xs) v =
  let (v', _) = foldr visibilityFun (v, -1) x
   in visibilityFold xs v'

visibilityFun :: Tree -> (Visibility, Int) -> (Visibility, Int)
visibilityFun (tree, pos) (vis, maxTree)
  | tree > maxTree = (Map.insert pos True vis, tree)
  | otherwise = (vis, maxTree)

createTreeGrid :: [[Int]] -> [[Tree]]
createTreeGrid xs = [[(v, (x, y)) | (x, v) <- zip [0 ..] row] | (y, row) <- zip [0 ..] xs]

generateVisibilityMap :: Int -> Int -> Visibility
generateVisibilityMap x y = Map.fromList [((i, j), False) | i <- [0 .. x - 1], j <- [0 .. y - 1]]

-- Parsers --
grid :: Parser [[Int]]
grid = endBy1 (many1 $ digitToInt <$> digit) spaces
