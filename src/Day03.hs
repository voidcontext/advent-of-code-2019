{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

day03Main :: IO ()
day03Main = do
  day03Part1Result <- day03Part1
  print day03Part1Result
  day03Part2Result <- day03Part2
  print day03Part2Result

debug :: a -> String ->  a
debug = flip trace

day03Part1 :: IO (Either String Int)
day03Part1 = do
  (x:y:_) <- loadFile "data/day03.input"
  return $ minManhattanDistance x y

day03Part2 :: IO (Either String Int)
day03Part2 = do
  (x:y:_) <- loadFile "data/day03.input"
  return $ minStepsToIntersection x y


minStepsToIntersection :: Text.Text -> Text.Text -> Either String Int
minStepsToIntersection a b = do
  ca <- parseWirePath a
  cb <- parseWirePath b
  return $ minimum $ (\p -> calcSteps ca p + calcSteps cb p) <$> intersections ca cb


minManhattanDistance :: Text.Text -> Text.Text -> Either String Int
minManhattanDistance a b = do
  ca <- parseWirePath a
  cb <- parseWirePath b
  return $ minimum (fmap manhattanDistanceFromStart (intersections ca cb))

parseWirePath :: Text.Text -> Either String WirePath
parseWirePath text =
  WirePath <$> traverse parseLine (Text.split (==',') text)
  where
    parseLine lineText = Path (directionFromText . Text.head $ lineText) <$> (fst <$> decimal (Text.tail lineText))
    directionFromText 'U' = UP
    directionFromText 'D' = DOWN
    directionFromText 'L' = LEFT
    directionFromText 'R' = RIGHT
    directionFromText _   = UP -- TODO: this needs to be fixed

intersections :: WirePath -> WirePath -> [(Int, Int)]
intersections wp1 wp2 =  nub . sort . filter (/= (0, 0)) . catMaybes $ [intersect' a b | a <- lines' wp1, b <- lines' wp2] 

manhattanDistanceFromStart :: (Int, Int) -> Int
manhattanDistanceFromStart (x, y) = abs x + abs y

center :: (Int, Int)
center = (0, 0)

lines' :: WirePath -> [Line]
lines' (WirePath ps) =  foldl toLine ([] :: [Line]) ps
  where toLine [] p = [Line center (pathFrom center p)]
        toLine ls p = ls ++ [Line (p2 . last $ ls) (pathFrom  (p2 . last $ ls) p)]
        pathFrom (x, y) (Path dir step) = (x + xDiff dir step, y + yDiff dir step)  

-- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
intersect' :: Line -> Line -> Maybe (Int, Int)
intersect' (Line (x1, y1) (x2, y2)) (Line (x3, y3) (x4, y4))
  | determinant == 0 = Nothing
  | otherwise        = intersectionPoint
  where
    determinant = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    intersectionPoint
      | 0 <= t && t <= 1 && 0 <= u && u <= 1 = Just  (x1 + floor (t * fi' (x2 - x1)), y1 + floor (t * fi' (y2  - y1)))
      | otherwise                            = Nothing
      where
        fi' = fromIntegral
        t = fi' ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / fi' determinant
        u = - (fi' ((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / fi' determinant)


calcSteps :: WirePath -> (Int, Int) -> Int
calcSteps wp point = moveAndCount (lines' wp) 0
  where
    moveAndCount [] _ = -1
    moveAndCount ls count
      | pointOnLine point h = count + distance (p1 h) point
      | otherwise           = moveAndCount (tail ls) (count + distance (p1 h) (p2 h))
      where h = head ls
    
pointOnLine :: (Int, Int) -> Line -> Bool
pointOnLine (a, b) (Line (x1, y1) (x2, y2))
  | x1 == x2 && x1 == a = min y1 y2 <= b && b <= max y1 y2 
  | y1 == y2 && y1 == b = min x1 x2 <= a && a <= max x1 x2
  | otherwise           = False

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 -x2) + abs (y1 - y2)

  
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
