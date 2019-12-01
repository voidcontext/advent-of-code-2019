module Lib
    ( day01
    ) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Text.Read

day01 :: IO (Either String Int)
day01 = do
  masses <- readIntsFromFile "data/day01.input"
  return $ calcConsumption masses
  where

calcConsumption :: Either String [Int] -> Either String Int 
calcConsumption (Right masses) = Right $ sum (map fuelConsumption masses)
calcConsumption (Left error) = Left error

fuelConsumption :: Int -> Int
fuelConsumption mass = (div mass 3) - 2

readIntsFromFile :: String -> IO (Either String [Int])
readIntsFromFile fileName = do 
  lines <- fmap Text.lines (Text.readFile fileName)
  return $ traverse id (map toDecimal lines)
  where
    toDecimal text = fmap (fst) (decimal text)
  
