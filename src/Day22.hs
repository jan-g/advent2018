module Day22
    ( day22
    , day22b
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


day22 :: [String] -> IO ()
day22 ls = do
  let result = parse ls
  putStrLn $ show result


{-

-}


day22b :: [String] -> IO ()
day22b ls = do
  let result = parse ls
  putStrLn $ show result
