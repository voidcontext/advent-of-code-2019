module Lib
    ( day01
    ) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Text.Read

day01 :: IO (Either String Int)
day01 = do
  readResult <- readIntsFromFile "data/day01.input"
  return $ fmap calcFuelConsumption readResult
  where
    calcFuelConsumption masses = sum $ (map fuelConsumption) masses
    fuelConsumption mass = (div mass 3) - 2

readIntsFromFile :: String -> IO (Either String [Int])
readIntsFromFile fileName = do 
  ls <- fmap Text.lines (Text.readFile fileName)
  return $ traverse id (map toDecimal ls)
  where
    toDecimal text = fmap (fst) (decimal text)
  
