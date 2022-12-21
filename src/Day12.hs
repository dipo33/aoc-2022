module Main where

import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import Data.Tuple.Extra (snd3)
import Text.Parsec (endBy1, letter, many1, spaces)
import Text.Parsec.String (Parser)
import qualified Utils

-- Data --
type Pos = (Int, Int)

type GridSquare = (Pos, Char, Int)

-- Solution --
main :: IO ()
main = do
  fullGraph <- buildGraph <$> Utils.parseFile heightmap "day12"
  let start = getByLabel 'S' fullGraph
  let end = getByLabel 'E' fullGraph

  let graph = Map.mapWithKey (filter . canClimb) fullGraph
  let path = Utils.bfsGetPath end $ Utils.bfs start end graph
  Utils.firstResult . (1 `subtract`) . length $ path

  let revGraph = Map.mapWithKey (filter . flip canClimb) fullGraph
  let predecessors = Utils.bfsAll end revGraph
  let squareA = filter ((== 'a') . getLabel) . Map.keys $ revGraph
  let lengths = filter (> 1) . map (length . flip Utils.bfsGetPath predecessors) $ squareA
  Utils.secondResult . (1 `subtract`) . minimum $ lengths
  where
    getByLabel :: Char -> Map GridSquare [GridSquare] -> GridSquare
    getByLabel label = head . filter ((== label) . getLabel) . Map.keys

    getLabel :: GridSquare -> Char
    getLabel = snd3

buildGraph :: [[Char]] -> Map GridSquare [GridSquare]
buildGraph hmap =
  let matrix = Matrix.fromLists [[((x, y), l, toHeight l) | (x, l) <- zip [1 ..] row] | (y, row) <- zip [1 ..] hmap]
      neighbours = (\o@(pos, _, _) -> (o, Utils.directMatrixNeighbors matrix pos)) <$> matrix
   in Map.fromList . Matrix.toList $ neighbours
  where
    toHeight :: Char -> Int
    toHeight 'S' = 0
    toHeight 'E' = 25
    toHeight c = ord c - 97

canClimb :: GridSquare -> GridSquare -> Bool
canClimb (_, _, from) (_, _, to) = to - from <= 1

-- Parsers --
heightmap :: Parser [[Char]]
heightmap = endBy1 (many1 letter) spaces
