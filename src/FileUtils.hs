module FileUtils (readIntsFromFile, readCommaSeparatedLineFromFile) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Text.Read

readCommaSeparatedLineFromFile :: String -> IO (Either String [Int])
readCommaSeparatedLineFromFile fileName = do
  ls <- fmap Text.lines (Text.readFile fileName)
  return $ parse ls
  where parse [l] = traverse toDecimal (Text.split (==',') l)
        parse _ = Left "There should be only 1 line"
            
readIntsFromFile :: String -> IO (Either String [Int])
readIntsFromFile fileName = do
  ls <- fmap Text.lines (Text.readFile fileName)
  return $ traverse toDecimal ls

toDecimal :: Text.Text -> Either String Int
toDecimal text = fst <$> decimal text
  
