module IntcodeComputer
  ( IntcodeProgram (IntcodeProgram)
  , memory
  , start
  , noun
  , verb
  ) where

newtype IntcodeProgram = IntcodeProgram  { memory :: [Int] }

start :: IntcodeProgram -> IntcodeProgram
start = runFrom 0

runFrom :: Int -> IntcodeProgram  -> IntcodeProgram
runFrom pointer program
  | opcode program pointer == 99 = program
  | otherwise                    = runFrom (pointer + 4) (runInstruction program pointer)


noun :: Int -> IntcodeProgram -> IntcodeProgram
noun val (IntcodeProgram m) = IntcodeProgram (replace m 1 val)

verb :: Int -> IntcodeProgram -> IntcodeProgram
verb val (IntcodeProgram m) = IntcodeProgram (replace m 2 val)


runInstruction :: IntcodeProgram -> Int -> IntcodeProgram
runInstruction program pointer = run' (instruction program pointer)
  where
    m                    = memory program
    run' (1:a:b:dest:_)  = IntcodeProgram $ replace m dest ((m !! a) + (m !! b))
    run' (2:a:b:dest:_)  = IntcodeProgram $ replace m dest ((m !! a) * (m !! b))
    run' _               = program

opcode :: IntcodeProgram -> Int -> Int
opcode program pointer = memory program !! pointer

instruction :: IntcodeProgram -> Int -> [Int]
instruction program pointer = drop pointer $ memory program

replace :: [Int] -> Int -> Int -> [Int]
replace xs ind value = take ind xs ++ [value] ++ drop (ind + 1) xs
