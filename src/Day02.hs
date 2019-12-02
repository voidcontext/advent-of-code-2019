module Day02 where

import FileUtils

data IntcodeProgram = IntcodeProgram  { memory :: [Int] }  

day02part1 :: IO (Either String [Int])
day02part1 = do
  lsOrError <- readCommaSeparatedLineFromFile "data/day02.input"
  return $ fmap (\ls -> memory $ start . (verb 2) . (noun 12) $ IntcodeProgram ls) lsOrError
  

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
