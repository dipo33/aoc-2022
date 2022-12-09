module Main where

import AdventParser (integer)
import Control.Applicative (liftA2)
import Data.List (partition)
import Text.Parsec (anyChar, char, many, many1, manyTill, newline, string, (<|>))
import Text.Parsec.String (Parser)
import qualified Utils

data FileNode = File String Int | Dir String [FileNode]
  deriving (Eq, Show)

data Command = CD String | LS [FileNode]
  deriving (Eq, Show)

main :: IO ()
main = do
  commands <- Utils.parseFile output "day07"
  let node = processCommands commands [] (Dir "/" [])
  Utils.firstResult . sum . filter (<= 100000) . getDirectorySizes $ node
  let requiredSpace = getSize node - 40000000
  Utils.secondResult . minimum . filter (>= requiredSpace) . getDirectorySizes $ node

processCommands :: [Command] -> [String] -> FileNode -> FileNode
processCommands [] _ node = node
processCommands ((CD "/") : cs) path node = processCommands cs path node
processCommands ((CD "..") : cs) path node = processCommands cs (drop 1 path) node
processCommands ((CD dir) : cs) path node = processCommands cs (dir : path) node
processCommands ((LS nodes) : cs) path node =
  let updatedNode = createNode node nodes (reverse path)
   in processCommands cs path updatedNode

createNode :: FileNode -> [FileNode] -> [String] -> FileNode
createNode (Dir name nodes) newNodes [] = Dir name $ newNodes ++ nodes
createNode (Dir name nodes) newNodes (path : paths) =
  let (dir, dirs) = partition ((== path) . getName) nodes
      updatedNode = createNode (head dir) newNodes paths
   in Dir name (updatedNode : dirs)
createNode _ _ _ = error "should not happen"

getSize :: FileNode -> Int
getSize (File _ x) = x
getSize (Dir _ xs) = sum . map getSize $ xs

getDirectorySizes :: FileNode -> [Int]
getDirectorySizes (File _ _) = []
getDirectorySizes dir@(Dir _ xs) = getSize dir : concatMap getDirectorySizes xs

getName :: FileNode -> String
getName (File name _) = name
getName (Dir name _) = name

-- Parsers --
output :: Parser [Command]
output = many1 $ string "$ " *> (changeDirectory <|> listFiles)

changeDirectory :: Parser Command
changeDirectory = CD <$> (string "cd " *> manyTill anyChar newline)

listFiles :: Parser Command
listFiles = LS <$> (string "ls" *> newline *> fileNodes)

fileNodes :: Parser [FileNode]
fileNodes = many $ directory <|> file

directory :: Parser FileNode
directory = flip Dir [] <$> (string "dir " *> manyTill anyChar newline)

file :: Parser FileNode
file = liftA2 (flip File) integer (char ' ' *> manyTill anyChar newline)
