module Day02 where

import FileUtils

day02 :: IO (Either String [Int])
day02 = do
  lsOrError <- readCommaSeparatedLineFromFile "data/day02.input"
  return $ fmap (\ls -> processList (replace (replace ls 1 12) 2 2)  0) lsOrError

processList :: [Int] -> Int -> [Int]
processList xs current
  | (head remaining) == 99 = xs
  | otherwise = processList (processOpcode remaining xs) (current + 1)
  where remaining = drop (current * 4) xs

processOpcode :: [Int] -> [Int] -> [Int]
processOpcode (1:a:b:dest:_) xs = replace xs dest ((xs !! a) + (xs !! b))
processOpcode (2:a:b:dest:_) xs = replace xs dest ((xs !! a) * (xs !! b))
processOpcode _ xs = xs 

replace :: [Int] -> Int -> Int -> [Int]
replace xs ind value = (take ind xs) ++ [value] ++ (drop (ind + 1) xs)
