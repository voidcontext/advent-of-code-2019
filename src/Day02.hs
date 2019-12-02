module Day02 where

import FileUtils

data IntcodeProgram = IntcodeProgram  { memory :: [Int] }  

day02part1 :: IO (Either String [Int])
day02part1 = do
  programOrError <- loadProgram
  return $ fmap (memory . start . verb 2 . noun 12) programOrError

day02part2 :: IO (Either String Int)
day02part2 = do
  programOrError <- loadProgram
  return $ fmap (answer . findNounAndVerb) programOrError
  where answer (n, v) = 100 * n + v
      

loadProgram :: IO (Either String IntcodeProgram)
loadProgram = do
  lsOrError <- readCommaSeparatedLineFromFile "data/day02.input"
  return $ fmap IntcodeProgram lsOrError

findNounAndVerb :: IntcodeProgram -> (Int, Int)
findNounAndVerb program = find 0 0
  where find n v
          | head result == 19690720 = (n, v)
          | n > 99 && v > 99        = (-1, -1)
          | v > 99                  = find (n + 1) 0
          | otherwise               = find n (v + 1)
          where result = memory . start . (verb v) . (noun n) $  program

noun :: Int -> IntcodeProgram -> IntcodeProgram
noun val (IntcodeProgram m) = IntcodeProgram (replace m 1 val)

verb :: Int -> IntcodeProgram -> IntcodeProgram
verb val (IntcodeProgram m) = IntcodeProgram (replace m 2 val)

start :: IntcodeProgram -> IntcodeProgram
start program = run 0 program 

run :: Int -> IntcodeProgram  -> IntcodeProgram
run pointer program
  | currentOpcode program pointer == 99 = program
  | otherwise                           = run (pointer + 4) (runInstruction program pointer)

runInstruction :: IntcodeProgram -> Int -> IntcodeProgram
runInstruction program pointer = run' (currentInstruction program pointer)
  where
    m                    = memory program
    run' (1:a:b:dest:_)  = IntcodeProgram $ replace m dest ((m !! a) + (m !! b))
    run' (2:a:b:dest:_)  = IntcodeProgram $ replace m dest ((m !! a) * (m !! b))
    run' _               = program
          

currentOpcode :: IntcodeProgram -> Int -> Int
currentOpcode program pointer = (memory program) !! pointer

currentInstruction :: IntcodeProgram -> Int -> [Int]
currentInstruction program pointer = drop pointer $ memory program
  
replace :: [Int] -> Int -> Int -> [Int]
replace xs ind value = (take ind xs) ++ [value] ++ (drop (ind + 1) xs)
