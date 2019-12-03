module FileUtils ( readIntsFromFile
                 , readCommaSeparatedLineFromFile
                 , loadFile
                 ) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Text.Read

readCommaSeparatedLineFromFile :: String -> IO (Either String [Int])
readCommaSeparatedLineFromFile fileName =  parse <$> loadFile fileName
  where parse [l] = traverse toDecimal (Text.split (==',') l)
        parse _ = Left "There should be only 1 line"
            
readIntsFromFile :: String -> IO (Either String [Int])
readIntsFromFile fileName = traverse toDecimal <$> loadFile fileName

loadFile :: String -> IO [Text.Text]
loadFile fileName = fmap Text.lines (Text.readFile fileName)

toDecimal :: Text.Text -> Either String Int
toDecimal text = fst <$> decimal text
  
