module Day04 where

import Data.Char

day04Main :: IO ()
day04Main = do
  print . length . filter id $ fmap (isPassword . toDigits) [start .. end]
  print . length . filter id $ fmap (isPassword' . toDigits) [start .. end]

start :: Int
start = 172930

end :: Int
end = 683082

toDigits :: Int -> [Int]
toDigits n = ord <$> show n

isPassword :: [Int] -> Bool
isPassword pw
  | length pw < 6 = False
  | otherwise     = hasAscendingDigits (head pw) 0 (tail pw)
  
isPassword' :: [Int] -> Bool
isPassword' pw = isPassword pw && any (\s -> length s == 2) (repeatingSubSequences pw)



hasAscendingDigits :: Int -> Int -> [Int] -> Bool
hasAscendingDigits _ doubles [] = doubles > 0
hasAscendingDigits l doubles ds
      | current < l  = False
      | current == l = hasAscendingDigits current (doubles + 1) $ tail ds
      | otherwise  = hasAscendingDigits current doubles $ tail ds
      where current =  head ds

repeatingSubSequences :: Eq a => [a] -> [[a]]
repeatingSubSequences []  = []
repeatingSubSequences (h:t) = addLast $ foldl compare' ([h], []) t
  where compare' (last', sequences) current
          | current == head last' = (last' ++ [current], sequences)
          | otherwise            = ([current], sequences ++ [last'])
        addLast (l, r) = r ++ [l]  
