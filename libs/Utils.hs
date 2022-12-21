module Utils where

import Control.Monad (guard)
import Data.Bool.HT (if')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

applyWhen :: (a -> Bool) -> (a -> a) -> a -> a
applyWhen when fun a = if' (when a) (fun a) a

applyFunctions2 :: [a -> b -> c] -> a -> b -> [c]
applyFunctions2 [] _ _ = []
applyFunctions2 (f : fs) a b = f a b : applyFunctions2 fs a b

mapWhen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhen when fun = map $ applyWhen when fun

readInput :: String -> IO String
readInput day = do
  args <- getArgs
  case args of
    [] -> readFile baseFile
    ["test"] -> readFile $ baseFile ++ "_test"
    [_] -> error "Invalid argument"
    _ -> error "Invalid number of arguments"
  where
    baseFile = "inputs/" ++ day ++ "/" ++ day

parseFile :: Parser a -> String -> IO a
parseFile parser day = do
  file <- Utils.readInput day
  case parse parser ("input:" ++ day) file of
    (Left err) -> error . show $ err
    (Right val) -> return val

firstResult :: Show a => a -> IO ()
firstResult = putStrLn . ("First Part: " ++) . show

secondResult :: Show a => a -> IO ()
secondResult = putStrLn . ("Second Part: " ++) . show

-- Containers --
directMatrixNeighbors :: Matrix a -> (Int, Int) -> [a]
directMatrixNeighbors m (x, y) =
  let rows = Matrix.nrows m
      cols = Matrix.ncols m
   in do
        (i, j) <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        guard $ and [i > 0, i <= cols, j > 0, j <= rows]
        return $ m Matrix.! (j, i)

-- Algorhitms --
bfsHelper :: (Eq a, Ord a) => a -> Maybe a -> Map a [a] -> Map a a
bfsHelper start end graph =
  let predecessors = bfsHelper' [start] $ Map.singleton start Nothing
   in fromJust <$> Map.filter isJust predecessors
  where
    bfsHelper' [] predecessors = predecessors
    bfsHelper' (x : xs) predecessors
      | Just x == end = predecessors
      | otherwise =
          let neighbours = filter (`Map.notMember` predecessors) $ graph Map.! x
           in bfsHelper' (xs ++ neighbours) (foldr (`Map.insert` Just x) predecessors neighbours)

bfs :: (Eq a, Ord a) => a -> a -> Map a [a] -> Map a a
bfs start end = bfsHelper start (Just end)

bfsAll :: (Eq a, Ord a) => a -> Map a [a] -> Map a a
bfsAll start = bfsHelper start Nothing

bfsGetPath :: (Ord a) => a -> Map a a -> [a]
bfsGetPath target predecessors = reverse . getPath $ target
  where
    getPath from = from : maybe [] getPath (Map.lookup from predecessors)
