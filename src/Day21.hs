module Day21
    ( day21
    , day21b
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


parse ls = "not yet"


day21 :: [String] -> IO ()
day21 ls = do
  let result = parse ls
  putStrLn $ show result


{-

-}


day21b :: [String] -> IO ()
day21b ls = do
  let result = parse ls
  putStrLn $ show result
