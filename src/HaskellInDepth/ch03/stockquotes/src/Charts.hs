module Charts where

import Data.Foldable (toList)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Backend.Diagrams

import QuoteData

plotChart :: Foldable t => String -> t QuoteData -> FilePath -> IO ()

