module FileUtils where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Text.Read

readIntsFromFile :: String -> IO (Either String [Int])
readIntsFromFile fileName = do 
  ls <- fmap Text.lines (Text.readFile fileName)
  return $ traverse id (map toDecimal ls)
  where
    toDecimal text = fmap (fst) (decimal text)
  
