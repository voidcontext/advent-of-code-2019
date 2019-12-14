{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import FileUtils


import qualified Data.Text as Text
import Data.Tuple
import qualified Data.Map.Strict as Map

type OrbitsMap = (Map.Map Text.Text Text.Text)

day06Main :: IO ()
day06Main = do
  ls <- loadFile "data/day06.input"
  print $ countAllOrbits . parse $ ls

countAllOrbits :: OrbitsMap -> Int
countAllOrbits os = sum . fmap (`countOrbits` os) .  Map.keys $ os

countOrbits :: Text.Text -> OrbitsMap -> Int
countOrbits name orbitsMap = count' (Map.lookup name orbitsMap)
  where count' (Just name') = 1 + countOrbits name' orbitsMap
        count' Nothing      = 0
        
parse :: [Text.Text] -> OrbitsMap
parse = Map.fromList . fmap (swap . trimSeparator . Text.breakOn ")")
  where trimSeparator (f, s) = (f, Text.tail s)


