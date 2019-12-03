module Day03Spec where

import Day03 hiding (length)

import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
  let wp1 = WirePath [Path RIGHT 8, Path UP 5, Path LEFT 5, Path DOWN 3]
  let wp2 = WirePath [Path UP 7, Path RIGHT 6, Path DOWN 4, Path LEFT 4]

  describe "parsePath" $ do
    it "should parse lines" $ do
      parseWirePath (Text.pack "U13,R12,D4,L1") `shouldBe` Right (WirePath [Path UP 13, Path RIGHT 12, Path DOWN 4, Path LEFT 1])

  describe "lines'" $ do
    it "should convert paths to lines" $ do
      lines' (WirePath [Path UP 3, Path LEFT 2, Path DOWN 4]) `shouldBe` [Line (0, 0) (0, 3), Line (0, 3) (-2, 3), Line (-2, 3) (-2, -1)]
      lines' wp1 `shouldBe` [Line (0, 0) (8, 0), Line (8, 0) (8, 5), Line (8, 5) (3, 5), Line (3, 5) (3, 2)]
      lines' wp2 `shouldBe` [Line (0, 0) (0, 7), Line (0, 7) (6, 7), Line (6, 7) (6, 3), Line (6, 3) (2, 3)]

  describe "intersections" $ do
    it "should return intersections" $ do
      intersections wp1 wp2 `shouldBe` [(3, 3), (6, 5)]

  describe "minManhattanDistance" $ do
    it "should return the correct answer 1" $ do
      minManhattanDistance (Text.pack "R75,D30,R83,U83,L12,D49,R71,U7,L72") (Text.pack "U62,R66,U55,R34,D71,R55,D58,R83") `shouldBe` Right 159
      minManhattanDistance (Text.pack "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51") (Text.pack "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") `shouldBe` Right 135
