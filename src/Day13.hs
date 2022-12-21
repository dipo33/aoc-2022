{-# LANGUAGE InstanceSigs #-}

module Main where

import AdventParser (integer)
import Control.Applicative (liftA2, (<|>))
import Data.Bifunctor (Bifunctor (first))
import Data.List (elemIndex, sort)
import Data.List.Extra (firstJust)
import Data.List.Index (indexed)
import Data.Maybe (fromJust, fromMaybe)
import Data.Maybe.HT (toMaybe)
import Text.Parsec (char, endBy1, sepBy, spaces)
import Text.Parsec.String (Parser)
import qualified Utils

-- Data --
data Packet a = PacketList [Packet a] | PacketValue a
  deriving (Eq)

instance (Show a) => Show (Packet a) where
  show :: Packet a -> String
  show (PacketValue x) = show x
  show (PacketList x) = show x

instance (Ord a) => Ord (Packet a) where
  compare :: Packet a -> Packet a -> Ordering
  x `compare` y = case check x y of
    (Just True) -> LT
    (Just False) -> GT
    Nothing -> EQ

-- Solution --
main :: IO ()
main = do
  pairs <- Utils.parseFile packetPairs "day13"
  Utils.firstResult . sum . map fst . filter (fromMaybe False . uncurry check . snd) . map (first (+ 1)) . indexed $ pairs
  let sorted = sort . (++ [firstDivider, secondDivider]) . foldr (\(a, b) acc -> a : b : acc) [] $ pairs
  Utils.secondResult $ (firstDivider `indexOf` sorted) * (secondDivider `indexOf` sorted)
  where
    firstDivider :: Packet Int
    firstDivider = PacketList [PacketList [PacketValue 2]]
    secondDivider :: Packet Int
    secondDivider = PacketList [PacketList [PacketValue 6]]
    indexOf :: (Eq a) => a -> [a] -> Int
    indexOf x = (+ 1) . fromJust . elemIndex x

check :: (Ord a) => Packet a -> Packet a -> Maybe Bool
check (PacketValue x) (PacketValue y) = toMaybe (x /= y) (x < y)
check (PacketList []) (PacketList []) = Nothing
check (PacketList []) (PacketList _) = Just True
check (PacketList _) (PacketList []) = Just False
check (PacketList (x : xs)) (PacketList (y : ys)) = firstJust id [check x y, check (PacketList xs) (PacketList ys)]
check v@(PacketValue _) list = check (PacketList [v]) list
check list v@(PacketValue _) = check list (PacketList [v])

-- Parsers --
packet :: Parser (Packet Int)
packet = PacketList <$> (char '[' *> sepBy listValue (char ',') <* char ']')
  where
    listValue :: Parser (Packet Int)
    listValue = packet <|> PacketValue <$> integer

packetPairs :: Parser [(Packet Int, Packet Int)]
packetPairs = endBy1 packetPair spaces
  where
    packetPair :: Parser (Packet Int, Packet Int)
    packetPair = liftA2 (,) packet $ spaces *> packet
