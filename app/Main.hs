module Main where

import Lib
import Day1
import Day2
import Day3
import Day4
import Day5

import System.Environment
import Data.Counter (Counter, empty, update, count, union, updateWith)
import qualified Data.Map.Strict (filter, size)
import Data.Map.Strict ((!), toList, foldlWithKey, filterWithKey)
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Function (on)
import Data.List



main = do
  args <- getArgs
  let action = case args !! 0 of
                 "day1" -> day1
                 "day1b" -> day1b
                 "day2" -> day2
                 "day2b" -> day2b
                 "day3" -> day3
                 "day3b" -> day3b
                 "day4" -> day4
                 "day4b" -> day4b
                 "day5" -> day5
                 "day5b" -> day5b
      source = args !! 1
  ls <- loadLines source
  action ls
