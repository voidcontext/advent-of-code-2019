module Day01 where

import FileUtils

day01part1 :: IO (Either String Int)
day01part1 = do
  readResult <- readIntsFromFile "data/day01.input"
  return $ fmap calcFuelConsumption readResult
  where
    calcFuelConsumption masses = sum $ map fuelConsumption masses

day01part2 :: IO (Either String Int)
day01part2 = do
  readResult <- readIntsFromFile "data/day01.input"
  return $ fmap calcFuelConsumption readResult
  where
    calcFuelConsumption masses = sum $ map totalFuelConsumption masses
    totalFuelConsumption mass
      | fuel <= 0 = 0
      | otherwise = fuel + totalFuelConsumption fuel
      where fuel = fuelConsumption mass
        

fuelConsumption :: Int -> Int
fuelConsumption mass = div mass 3 - 2

