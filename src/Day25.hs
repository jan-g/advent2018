module Day25
    ( day25
    , day25b
    ) where

import Lib
import Text.ParserCombinators.ReadP
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Array ((!))
import Data.Foldable
import qualified Data.List.Split as Split
import Data.Bits
import Data.Char


{-

-}


parse ls = "Don't advent calendars usually only run for 24 days?"


day25 :: [String] -> IO ()
day25 ls = do
  let result = parse ls
  putStrLn $ show result


{-

-}


day25b :: [String] -> IO ()
day25b ls = do
  let result = parse ls
  putStrLn $ show result
