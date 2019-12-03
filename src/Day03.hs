{-# LANGUAGE OverloadedStrings #-}

module Day03  where

import FileUtils

import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Read
import Debug.Trace

data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Eq, Show)

data Path = Path
  { direction :: Direction
  , length :: Int
  } deriving (Eq, Show)

data Line = Line { p1 :: (Int, Int), p2 :: (Int, Int) } deriving (Eq, Show)

newtype WirePath = WirePath { paths :: [Path]} deriving (Eq, Show)

debug :: a -> String ->  a
debug = flip trace

day03 :: IO (Either String Int)
day03 = do
  (x:y:_) <- loadFile "data/day03.input"
  return $ minManhattanDistance x y

minManhattanDistance :: Text.Text -> Text.Text -> Either String Int
minManhattanDistance a b = do
  ca <- parseWirePath a
  cb <- parseWirePath b
  return $ minimum (fmap manhattanDistanceFromStart (intersections ca cb))

parseWirePath :: Text.Text -> Either String WirePath
parseWirePath text =
  WirePath <$> traverse parseLine (Text.split (==',') text)
  where
    parseLine lineText = fmap (\l -> Path (directionFromText . Text.head $ lineText) l) $ (fst <$> decimal (Text.tail lineText))
    directionFromText 'U' = UP
    directionFromText 'D' = DOWN
    directionFromText 'L' = LEFT
    directionFromText 'R' = RIGHT
    directionFromText _   = UP -- TODO: this needs to be fixed

intersections :: WirePath -> WirePath -> [(Int, Int)]
intersections wp1 wp2 =  nub . sort . (filter (/= (0, 0))) . catMaybes $ [intersect' a b | a <- lines' wp1, b <- lines' wp2] 

manhattanDistanceFromStart :: (Int, Int) -> Int
manhattanDistanceFromStart (x, y) = abs x + (abs y)

center :: (Int, Int)
center = (0, 0)

lines' :: WirePath -> [Line]
lines' (WirePath ps) =  foldl toLine ([] :: [Line]) ps
  where toLine [] p = [Line center (pathFrom center p)]
        toLine ls p = ls ++ [Line (p2 . last $ ls) (pathFrom  (p2 . last $ ls) p)]
        pathFrom (x, y) (Path dir step) = (x + xDiff dir step, y + yDiff dir step)  

intersect' :: Line -> Line -> Maybe (Int, Int)
intersect' (Line a b) (Line c d)
  | determinant == 0 = Nothing
  | otherwise        = intersectionPoint
  where a1 = snd b - snd a
        b1 = fst a - fst b
        c1 = a1 * (fst a) + b1 * (snd a)
        a2 = snd d - snd c
        b2 = fst c - fst d
        c2 = a2 * (fst c) + b2 * (snd c)
        determinant = a1 * b2 - a2 * b1
        intersectionPoint
          | pointOnSegment a b && pointOnSegment c d = Just (x, y)
          | otherwise                                = Nothing
          where x = (c1 * b2 - c2 * b1) `quot` determinant
                y = (a1 * c1 - a2 * c1) `quot` determinant
                pointOnSegment (x1, y1) (x2, y2) =
                  min x1 x2 <= x && x <= max x1 x2 &&
                  min y1 y2 <= y && y <= max y1 y2 

xDiff :: Direction -> Int -> Int
xDiff UP _    = 0
xDiff DOWN _  = 0
xDiff LEFT s  = -s
xDiff RIGHT s = s

yDiff :: Direction -> Int -> Int
yDiff UP s    = s
yDiff DOWN s  = -s
yDiff LEFT _  = 0
yDiff RIGHT _ = 0
