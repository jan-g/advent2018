module Day18
    ( day18
    , day18b
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


{-
On the outskirts of the North Pole base construction project, many Elves are collecting lumber.

The lumber collection area is 50 acres by 50 acres; each acre can be either open ground (.), trees (|), or a lumberyard
(#). You take a scan of the area (your puzzle input).

Strange magic is at work here: each minute, the landscape looks entirely different. In exactly one minute, an open acre
can fill with trees, a wooded acre can be converted to a lumberyard, or a lumberyard can be cleared to open ground (the
lumber having been sent to other projects).

The change to each acre is based entirely on the contents of that acre as well as the number of open, wooded, or
lumberyard acres adjacent to it at the start of each minute. Here, "adjacent" means any of the eight acres surrounding
that acre. (Acres on the edges of the lumber collection area might have fewer than eight adjacent acres; the missing
acres aren't counted.)

In particular:

    An open acre will become filled with trees if three or more adjacent acres contained trees. Otherwise, nothing
      happens.
    An acre filled with trees will become a lumberyard if three or more adjacent acres were lumberyards. Otherwise,
      nothing happens.
    An acre containing a lumberyard will remain a lumberyard if it was adjacent to at least one other lumberyard and
      at least one acre containing trees. Otherwise, it becomes open.

These changes happen across all acres simultaneously, each of them using the state of all acres at the beginning of
the minute and changing to their new form by the end of that same minute. Changes that happen during the minute don't
affect each other.

For example, suppose the lumber collection area is instead only 10 by 10 acres with this initial configuration:

Initial state:
.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.

After 1 minute:
.......##.
......|###
.|..|...#.
..|#||...#
..##||.|#|
...#||||..
||...|||..
|||||.||.|
||||||||||
....||..|.

After 2 minutes:
.......#..
......|#..
.|.|||....
..##|||..#
..###|||#|
...#|||||.
|||||||||.
||||||||||
||||||||||
.|||||||||

After 3 minutes:
.......#..
....|||#..
.|.||||...
..###|||.#
...##|||#|
.||##|||||
||||||||||
||||||||||
||||||||||
||||||||||

After 4 minutes:
.....|.#..
...||||#..
.|.#||||..
..###||||#
...###||#|
|||##|||||
||||||||||
||||||||||
||||||||||
||||||||||

After 5 minutes:
....|||#..
...||||#..
.|.##||||.
..####|||#
.|.###||#|
|||###||||
||||||||||
||||||||||
||||||||||
||||||||||

After 6 minutes:
...||||#..
...||||#..
.|.###|||.
..#.##|||#
|||#.##|#|
|||###||||
||||#|||||
||||||||||
||||||||||
||||||||||

After 7 minutes:
...||||#..
..||#|##..
.|.####||.
||#..##||#
||##.##|#|
|||####|||
|||###||||
||||||||||
||||||||||
||||||||||

After 8 minutes:
..||||##..
..|#####..
|||#####|.
||#...##|#
||##..###|
||##.###||
|||####|||
||||#|||||
||||||||||
||||||||||

After 9 minutes:
..||###...
.||#####..
||##...##.
||#....###
|##....##|
||##..###|
||######||
|||###||||
||||||||||
||||||||||

After 10 minutes:
.||##.....
||###.....
||##......
|##.....##
|##.....##
|##....##|
||##.####|
||#####|||
||||#|||||
||||||||||

After 10 minutes, there are 37 wooded acres and 31 lumberyards. Multiplying the number of wooded acres by the number of lumberyards gives the total resource value after ten minutes: 37 * 31 = 1147.

What will the total resource value of the lumber collection area be after 10 minutes?
-}

parse :: [String] -> Array.Array (Int, Int) Char
parse ls =
  let dy = length ls
      dx = length $ ls !! 0
  in  Array.array ((0,0), (dx-1, dy-1)) [((x,y),c) | (y,line) <- zip [0..] ls, (x,c) <- zip [0..] line]

d0 = lines "\
  \.#.#...|#.\n\
  \.....#|##|\n\
  \.|..|...#.\n\
  \..|#.....#\n\
  \#.#|||#|#|\n\
  \...#.||...\n\
  \.|....|...\n\
  \||...#|.#|\n\
  \|.||||..|.\n\
  \...#.|..|.\n\
  \"

showArea a =
  let ((xmin, ymin), (xmax, ymax)) = Array.bounds a
  in  unlines [[a ! (x,y) | x <- [xmin..xmax]] | y <- [ymin..ymax]]


infix 8 @@

a @@ (x,y) =
  let ((xmin, ymin), (xmax, ymax)) = Array.bounds a
  in  if x < xmin || x > xmax || y < ymin || y > ymax
      then '.'
      else a ! (x,y)

day18 :: [String] -> IO ()
day18 ls = do
  let area0 = parse ls
  putStrLn $ showArea area0
  area <- run 10 area0
  let (t, l) = score area
  putStrLn $ "The area has " ++ (show t) ++ " trees and " ++ (show l) ++ " lumberyards"
  putStrLn $ "Total score: " ++ (show $ t * l)

run 0 area = return area
run n area = do
  let area' = next area
  putStrLn $ "\ESC[H" ++ showArea area'
  run (n-1) area'

trees a = length $ filter (=='|') $ Array.elems a
lumberyards a = length $ filter (=='#') $ Array.elems a
score a = (trees a, lumberyards a)

charsAround c a x y =
  (if a @@ (x-1, y-1) == c then 1 else 0) +
  (if a @@ (x, y-1) == c then 1 else 0) +
  (if a @@ (x+1, y-1) == c then 1 else 0) +
  (if a @@ (x-1, y) == c then 1 else 0) +
  (if a @@ (x+1, y) == c then 1 else 0) +
  (if a @@ (x-1, y+1) == c then 1 else 0) +
  (if a @@ (x, y+1) == c then 1 else 0) +
  (if a @@ (x+1, y+1) == c then 1 else 0)

treesAround = charsAround '|'
lumberyardsAround = charsAround '#'

next :: Array.Array (Int,Int) Char -> Array.Array (Int,Int) Char
next a =
  let ((xmin, ymin), (xmax, ymax)) = Array.bounds a
  in  Array.array ((xmin, ymin), (xmax, ymax))
      [((x, y), c') | ((x,y),c) <- Array.assocs a,
                     let t = treesAround a x y
                         l = lumberyardsAround a x y
                         c' = case c of
                              '.' -> if t >= 3 then '|' else c
                              '|' -> if l >= 3 then '#' else c
                              '#' -> if l < 1 || t < 1 then '.' else c
                     ]


{-
This important natural resource will need to last for at least thousands of years. Are the Elves collecting this lumber
sustainably?

What will the total resource value of the lumber collection area be after 1000000000 minutes?
-}

day18b :: [String] -> IO ()
day18b ls = do
  let area0 = parse ls
  putStrLn $ showArea area0
  (area,scores) <- run' 3500 area0 []
  putStrLn $ showArea area
  putStrLn $ show scores
  putStrLn $ show $ drop 35 scores
  (final, scores') <- run' ((1000000000-3500) `mod` 35) area []
  let (t, l) = score final
  putStrLn $ "The area has " ++ (show t) ++ " trees and " ++ (show l) ++ " lumberyards"
  putStrLn $ "Total score: " ++ (show $ t * l)

run' 0 area scores = return (area,scores)
run' n area scores = do
  let area' = next area
      (t, l) = score area'

  run' (n-1) area' ((t,l):scores)
