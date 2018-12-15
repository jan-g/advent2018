module Lib
    ( loadLines
    , intParser
    , natParser
    ) where

import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char

loadLines :: String -> IO [String]
loadLines fn = do
  contents <- readFile fn
  return (lines contents)

natParser :: ReadP Integer
natParser = do
  digits <- many1 $ satisfy (isDigit)
  return $ read digits

intParser :: ReadP Integer
intParser = do
  (do
    char '-'
    i <- natParser
    return $ -i) <++ natParser