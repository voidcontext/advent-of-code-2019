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

data IntcodeProgram = IntcodeProgram
  { memory :: [Int]
  , pointer :: Int
  , input :: Maybe Int
  , output :: [Int]
  } deriving (Eq, Show)

data ParameterMode = Position | Value deriving (Eq, Show)

data Opcode =
  Add { param1Mode :: ParameterMode, param2Mode :: ParameterMode, param3Mode :: ParameterMode } |
  Mul { param1Mode :: ParameterMode, param2Mode :: ParameterMode, param3Mode :: ParameterMode } |
  Input |
  Output | 
  End
  deriving (Eq, Show)

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
evalInstruction p'@(IntcodeProgram m p i o) (Add p1m p2m _) =
  IntcodeProgram (replace m (getParam p' 3 Value) ((getParam p' 1 p1m) + (getParam p' 2 p2m))) (p + 4) i o
evalInstruction p'@(IntcodeProgram m p i o) (Mul p1m p2m _) =
  IntcodeProgram (replace m (getParam p' 3 Value) ((getParam p' 1 p1m) * (getParam p' 2 p2m))) (p + 4) i o
evalInstruction p'@(IntcodeProgram m p i o) Input =
  IntcodeProgram (replace m (getParam p' 1 Value) (fromJust i)) (p + 2) i o
evalInstruction p'@(IntcodeProgram m p i o) Output =
  IntcodeProgram m (p + 2) i (o ++ [getParam p' 1 Position])
evalInstruction program End = program

getParam :: IntcodeProgram -> Int -> ParameterMode -> Int
getParam (IntcodeProgram m p _ _) offset Position = m !! (m !! (p + offset))
getParam (IntcodeProgram m p _ _) offset Value    = m !! (p + offset) 
      
opcode :: IntcodeProgram -> Either String Opcode
opcode program 
  | opc == 99          = Right End
  | opc `mod` 100 == 1 = do
      param1Mode' <- parameterMode opc 1
      param2Mode' <- parameterMode opc 2
      param3Mode' <- parameterMode opc 3
      return $ Add param1Mode' param2Mode' param3Mode'
  | opc `mod` 100 == 2 = do
      param1Mode' <- parameterMode opc 1
      param2Mode' <- parameterMode opc 2
      param3Mode' <- parameterMode opc 3
      return $ Mul param1Mode' param2Mode' param3Mode'
  | opc `mod` 100 == 3 = Right Input   
  | opc `mod` 100 == 4 = Right Output   
  | otherwise          = Left "Unknown opcode"    
  where opc = memory program !! pointer program

parameterMode :: Int -> Int -> Either String ParameterMode
parameterMode n p
  | mode == 0 = Right Position
  | mode == 1 = Right Value
  | otherwise = Left "Unknown parameter mode"
  where mode = (n `mod` (10 ^ (p + 2)))  `div` (10 ^ (p + 1)) 

replace :: [Int] -> Int -> Int -> [Int]
replace xs ind value = take ind xs ++ [value] ++ drop (ind + 1) xs
