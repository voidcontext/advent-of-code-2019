module Day02 where

import FileUtils
import IntcodeComputer

day02Main :: IO ()
day02Main = do
  day02Part1Result <- day02part1
  putStrLn $ show day02Part1Result
  day02Part2Result <- day02part2
  putStrLn $ show day02Part2Result

day02part1 :: IO (Either String [Int])
day02part1 =  (>>= extract . run) <$> loadProgram
  where run = runProgram . verb 2 . noun 12
        extract = fmap memory

day02part2 :: IO (Either String (Maybe Int))
day02part2 = (>>= (fmap (fmap answer) . findNounAndVerb)) <$> loadProgram
  where answer (n, v) = 100 * n + v


loadProgram :: IO (Either String IntcodeProgram)
loadProgram = do
  lsOrError <- readCommaSeparatedLineFromFile "data/day02.input"
  return $ fmap (\m -> IntcodeProgram m 0 Nothing []) lsOrError

findNounAndVerb :: IntcodeProgram -> Either String (Maybe (Int, Int))
findNounAndVerb program = find 0 0
  where find n v =  (fmap memory . runProgram . verb v . noun n $ program) >>= f'
          where
            f' result
              | head result == 19690720 = Right $ Just (n, v)
              | n > 99 && v > 99        = Right Nothing
              | v > 99                  = find (n + 1) 0
              | otherwise               = find n (v + 1)

