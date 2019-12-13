module IntcodeComputer
  -- ( IntcodeProgram (IntcodeProgram)
  -- , parse
  -- , memory
  -- , runProgram
  -- , noun
  -- , verb
  -- )
where

import Data.Maybe
import qualified Data.Text    as Text
import Data.Text.Read
import Debug.Trace

data IntcodeProgram = IntcodeProgram
  { memory :: [Int]
  , pointer :: Int
  , input :: Maybe Int
  , output :: [Int]
  } deriving (Eq, Show)

data ParameterMode = Position | Value deriving (Eq, Show)

data Opcode =
  Add |
  Mul |
  Input |
  Output |
  JumpIfTrue |
  JumpIfFalse |
  LessThan |
  Equals |
  End
  deriving (Eq, Show)

debug :: a -> String ->  a
debug = flip trace

parse :: Text.Text -> Either String IntcodeProgram
parse program = do
  parsedProgram <- traverse toDecimal (Text.split (==',') program)
  return $ IntcodeProgram parsedProgram 0 Nothing []

toDecimal :: Text.Text -> Either String Int
toDecimal text = fst <$> decimal text

noun :: Int -> IntcodeProgram -> IntcodeProgram
noun val (IntcodeProgram m p i o) = IntcodeProgram (replace m 1 val) p i o

verb :: Int -> IntcodeProgram -> IntcodeProgram
verb val (IntcodeProgram m p i o) = IntcodeProgram (replace m 2 val) p i o

runProgram ::  IntcodeProgram  -> Either String IntcodeProgram
runProgram program = do
  opcode' <-  opcode program 
  runOrExit (evalInstruction program opcode') opcode'
  where runOrExit program' lastOpcode
          | lastOpcode == End = Right program'
          | otherwise         = runProgram program'



evalInstruction :: IntcodeProgram -> Opcode -> IntcodeProgram
evalInstruction program End                        = program
evalInstruction p Add                              = incrementPointerBy 4 . modifyMemory (binaryOp (+) p) $ p
evalInstruction p Mul                              = incrementPointerBy 4 . modifyMemory (binaryOp (*) p) $ p
evalInstruction p Input                            = incrementPointerBy 2 . modifyMemory (replace' (paramValue p 1) (fromJust $ input p)) $ p
evalInstruction p'@(IntcodeProgram m p i o) Output = IntcodeProgram m (p + 2) i (o ++ [param p' 1])
evalInstruction p JumpIfTrue                       = if test (/= 0) p then movePointer (param p 2) p else incrementPointerBy 3 p
evalInstruction p  JumpIfFalse                     = if test (== 0) p then movePointer (param p 2) p else incrementPointerBy 3 p
evalInstruction p LessThan                         = incrementPointerBy 4 .modifyMemory (binaryOp (\p1 p2 -> if p1 < p2 then 1 else 0) p) $ p 
evalInstruction p Equals                           = incrementPointerBy 4 .modifyMemory (binaryOp (\p1 p2 -> if p1 == p2 then 1 else 0) p) $ p 

binaryOp :: (Int -> Int -> Int) -> IntcodeProgram -> ([Int] -> [Int])
binaryOp f program = replace' (paramValue program 3) (f (param program 1) (param program 2))

test :: (Int -> Bool) -> IntcodeProgram -> Bool
test f program = f (param program 1) 

modifyMemory :: ([Int] -> [Int]) -> IntcodeProgram -> IntcodeProgram
modifyMemory f (IntcodeProgram m p i o) = IntcodeProgram (f m) p i o

movePointer :: Int -> IntcodeProgram -> IntcodeProgram
movePointer address (IntcodeProgram m _ i o) = IntcodeProgram m address i o

incrementPointerBy :: Int -> IntcodeProgram -> IntcodeProgram
incrementPointerBy step (IntcodeProgram m p i o) = IntcodeProgram m (p + step) i o

param :: IntcodeProgram -> Int -> Int
param program offset = param' pm
  where pm = paramMode offset program
        pv = paramValue program offset
        param' Position = (!! pv) . memory $  program
        param' Value    = pv

paramValue :: IntcodeProgram -> Int -> Int
paramValue (IntcodeProgram m p _ _) offset = m !! (p + offset)

paramMode :: Int -> IntcodeProgram -> ParameterMode
paramMode parameter (IntcodeProgram m p _ _)
  | mode == 0 = Position
  | otherwise = Value
  where opc  = m !! p
        mode = (opc `mod` (10 ^ (parameter + 2)))  `div` (10 ^ (parameter + 1)) 

opcode :: IntcodeProgram -> Either String Opcode
opcode program 
  | opc == 99          = Right End
  | opc `mod` 100 == 1 = Right Add
  | opc `mod` 100 == 2 = Right Mul
  | opc `mod` 100 == 3 = Right Input   
  | opc `mod` 100 == 4 = Right Output   
  | opc `mod` 100 == 5 = Right JumpIfTrue   
  | opc `mod` 100 == 6 = Right JumpIfFalse   
  | opc `mod` 100 == 7 = Right LessThan
  | opc `mod` 100 == 8 = Right Equals 
  | otherwise          = Left "Unknown opcode"    
  where opc = memory program !! pointer program

replace :: [Int] -> Int -> Int -> [Int]
replace xs ind value = take ind xs ++ [value] ++ drop (ind + 1) xs

replace' :: Int -> Int -> [Int] -> [Int]
replace' ind value xs = take ind xs ++ [value] ++ drop (ind + 1) xs
