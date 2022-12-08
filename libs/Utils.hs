module Utils where

import Data.Bool.HT (if')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

applyWhen :: (a -> Bool) -> (a -> a) -> a -> a
applyWhen when fun a = if' (when a) (fun a) a

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

mapUpdateLookup :: Ord k => (a -> a) -> k -> Map k a -> (a, Map k a)
mapUpdateLookup fun key oldMap =
  let (val, newMap) = Map.updateLookupWithKey (\_ -> Just . fun) key oldMap
   in (fromJust val, newMap)