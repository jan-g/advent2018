module Lib
    ( loadLines
    , intParser
    , natParser
    , forAllIO
    , dropWhileR
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

forAllIO :: [a] -> (a -> IO b) -> IO b
forAllIO [a] f = f a
forAllIO (a:as) f = do
  f a
  forAllIO as f

dropWhileR p as = reverse $ dropWhile p $ reverse as