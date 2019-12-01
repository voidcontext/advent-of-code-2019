module Main where

import Lib

main :: IO ()
main = do
  result <- day01
  putStrLn $ show result 
