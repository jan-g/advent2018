module Lib
    ( loadLines
    , intParser
    ) where

import System.Environment
import Text.Read
import Text.ParserCombinators.ReadP
import Data.Char

loadLines :: String -> IO [String]
loadLines fn = do
  contents <- readFile fn
  return (lines contents)

intParser :: ReadP Integer
intParser = do
  digits <- many1 $ satisfy (isDigit)
  return $ read digits