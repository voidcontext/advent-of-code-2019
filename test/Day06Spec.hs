{-# LANGUAGE OverloadedStrings #-}

module Day06Spec where

import Day06


import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = do
  let data' = ["COM)B", "B)C","C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]
  describe "parse" $ do
    it "should parse lines" $ do
      parse ["COM)B", "B)C"] `shouldBe` Map.fromList [("B", "COM"), ("C", "B")]

  describe "countOrbits" $ do
    let orbits = parse data'
    it "should calculate orbits properly" $ do
        countOrbits "D" orbits `shouldBe` 3
        countOrbits "L" orbits `shouldBe` 7

  describe "countAllOrbits" $ do
    let orbits = parse data'
    it "should count all orbits" $ do
        countAllOrbits orbits `shouldBe` 42
    
