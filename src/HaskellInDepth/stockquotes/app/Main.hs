module Main where

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)

import QuoteData

main :: IO ()
main = putStrLn "Stock quotes processing app"

--work :: Params -> IO ()

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
    csvData <- BL.readFile fpath
    case decodeByName csvData of
        Left err -> error err
        Right (_,quotes) -> pure (toList quotes)
