module Day3 where

import Data.Counter (Counter, empty, update, count, union)
import qualified Data.Map.Strict (filter, size)
import Data.Map.Strict ((!))
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe


{-
The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit (thanks to someone who helpfully wrote
its box IDs on the wall of the warehouse in the middle of the night). Unfortunately, anomalies are still affecting them
- nobody can even agree on how to cut the fabric.

The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.

Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims have an ID and consist
of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:

    The number of inches between the left edge of the fabric and the left edge of the rectangle.
    The number of inches between the top edge of the fabric and the top edge of the rectangle.
    The width of the rectangle in inches.
    The height of the rectangle in inches.

A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from
the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of fabric represented by # (and
ignores the square inches of fabric represented by .) in the diagram below:

...........
...........
...#####...
...#####...
...#####...
...#####...
...........
...........
...........

The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas. For example,
consider the following claims:

#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2

Visually, these claim the following areas:

........
...2222.
...2222.
.11XX22.
.11XX22.
.111133.
.111133.
........

The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the others, does not
overlap either of them.)

If the Elves all proceed with their own plans, none of them will have enough fabric. How many square inches of fabric
are within two or more claims?
-}

day3 ls = do
  {- let ls = ["#1 @ 1,3: 4x4","#2 @ 3,1: 4x4","#3 @ 5,5: 2x2"] -}
  putStrLn ("number of lines, raw = " ++ (show $ length ls))
  let rs = catMaybes $ map parseRect ls
      blank = Data.Counter.empty :: Counter (Integer, Integer) Integer
      chopped = foldl (chop) blank rs
      result = Data.Map.Strict.size $ Data.Map.Strict.filter (> 1) chopped
  putStrLn ("runmber of lines, parsed = " ++ (show $ length rs))
  putStrLn (show result)

data Rect = Rect { identifier :: Integer
                 , top :: Integer
                 , left :: Integer
                 , width :: Integer
                 , height :: Integer
                 }
            deriving (Show)


parseRect s =
  case readP_to_S rectParser s of
    [] -> Nothing
    [(r, "")] -> Just r

rectParser :: ReadP Rect
rectParser = do
  char '#'
  id <- intParser
  skipSpaces
  char '@'
  skipSpaces
  x <- intParser
  char ','
  y <- intParser
  char ':'
  skipSpaces
  w <- intParser
  char 'x'
  h <- intParser
  eof
  return Rect { identifier = id
              , top = y
              , left = x
              , width = w
              , height = h
              }

intParser :: ReadP Integer
intParser = do
  digits <- many1 $ satisfy (isDigit)
  return $ read digits



{-
Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single square inch of fabric with any
other claim. If you can somehow draw attention to it, maybe the Elves will be able to make Santa's suit after all!

For example, in the claims above, only claim 3 is intact after all claims are made.

What is the ID of the only claim that doesn't overlap?
-}

day3b ls = do
  {- let ls = ["#1 @ 1,3: 4x4","#2 @ 3,1: 4x4","#3 @ 5,5: 2x2"] -}
  putStrLn ("number of lines, raw = " ++ (show $ length ls))
  let rs = catMaybes $ map parseRect ls
      blank = Data.Counter.empty :: Counter (Integer, Integer) Integer
      chopped = foldl (chop) blank rs
      result = filter (allOnes chopped) rs
  putStrLn ("runmber of lines, parsed = " ++ (show $ length rs))
  putStrLn (show result)


chop :: Counter (Integer, Integer) Integer -> Rect -> Counter (Integer, Integer) Integer
chop layout rect =
  let region = Data.Counter.count $ makeRegion rect
  in  union region layout

makeRegion Rect {top=t, left=l, width=w, height=h} = [(x + l, y + t) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

allOnes :: Counter (Integer, Integer) Integer -> Rect -> Bool
allOnes chopped rect =
  let region = makeRegion rect
  in  all (\(x, y) -> 1 == chopped ! (x, y)) region

