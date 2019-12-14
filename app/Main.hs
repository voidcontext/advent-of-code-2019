module Main where

import Lib

import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  runMain args


runMain :: [String] -> IO ()
runMain ["1"]   = day01Main
runMain ["2"]   = day02Main
runMain ["3"]   = day03Main
runMain ["4"]   = day04Main
runMain ["5"]   = day05Main
runMain ["6"]   = day06Main
runMain []      = putStrLn "Missing day number!"     >>= failure
runMain [_]     = putStrLn "Unimplemented solution!" >>= failure
runMain (_:_:_) = putStrLn "Too many params!"        >>= failure

failure :: a -> IO a
failure _ = exitWith $ ExitFailure 1
