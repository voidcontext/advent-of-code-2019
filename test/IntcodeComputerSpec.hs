{-# LANGUAGE OverloadedStrings #-}

module IntcodeComputerSpec where

import IntcodeComputer

import qualified Data.Text as Text ()
import Test.Hspec

spec :: Spec
spec = do
  let puzzleInput = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0]
  
  describe "parse" $ do
    it "should parse programs correctly" $ do
      parse "1,9,10,3,2,3,11,0,99,30,40,50"  `shouldBe` (Right $ IntcodeProgram [1,9,10,3,2,3,11,0,99,30,40,50] 0 Nothing [])

  describe "start" $ do
    it "should run day02 program" $ do
      ((fmap memory) . runProgram $ IntcodeProgram [1,9,10,3,2,3,11,0,99,30,40,50] 0 Nothing []) `shouldBe` Right [3500,9,10,70,2,3,11,0,99,30,40,50]

    it "should run day02 puzzle part 1" $ do
      (fmap  (head . memory) . runProgram  . noun 12 . verb 2 $ IntcodeProgram puzzleInput 0 Nothing []) `shouldBe` Right 2890696

    it "should handle parameter modes" $ do
      ((fmap memory ) . runProgram  $ IntcodeProgram [1002,4,3,4,33] 0 Nothing []) `shouldBe` Right [1002,4,3,4,99]

    it "should handle input" $ do
      ((fmap memory ) . runProgram  $ IntcodeProgram [3,2,0] 0 (Just 99) []) `shouldBe` Right [3,2,99]

    it "should handle output" $ do
      ((fmap output ) . runProgram  $ IntcodeProgram [3,4,4,4,0] 0 (Just 99) []) `shouldBe` Right [99]
