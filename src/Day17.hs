module Day17
    ( day17
    , day17b
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
You arrive in the year 18. If it weren't for the coat you got in 1018, you would be very cold: the North Pole base
hasn't even been constructed.

Rather, it hasn't been constructed yet. The Elves are making a little progress, but there's not a lot of liquid water
in this climate, so they're getting very dehydrated. Maybe there's more underground?

You scan a two-dimensional vertical slice of the ground nearby and discover that it is mostly sand with veins of clay.
The scan only provides data with a granularity of square meters, but it should be good enough to determine how much
water is trapped there. In the scan, x represents the distance to the right, and y represents the distance down. There
is also a spring of water near the surface at x=500, y=0. The scan identifies which square meters are clay (your puzzle
input).

For example, suppose your scan shows the following veins of clay:

x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504

Rendering clay as #, sand as ., and the water spring as +, and with x increasing to the right and y increasing downward,
this becomes:

   44444455555555
   99999900000000
   45678901234567
 0 ......+.......
 1 ............#.
 2 .#..#.......#.
 3 .#..#..#......
 4 .#..#..#......
 5 .#.....#......
 6 .#.....#......
 7 .#######......
 8 ..............
 9 ..............
10 ....#.....#...
11 ....#.....#...
12 ....#.....#...
13 ....#######...

The spring of water will produce water forever. Water can move through sand, but is blocked by clay. Water always moves
down when possible, and spreads to the left and right otherwise, filling space that has clay on both sides and falling
out otherwise.

For example, if five squares of water are created, they will flow downward until they reach the clay and settle there.
Water that has come to rest is shown here as ~, while sand through which water has passed (but which is now dry again)
is shown as |:

......+.......
......|.....#.
.#..#.|.....#.
.#..#.|#......
.#..#.|#......
.#....|#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...

Two squares of water can't occupy the same location. If another five squares of water are created, they will settle on
the first five, filling the clay reservoir a little more:

......+.......
......|.....#.
.#..#.|.....#.
.#..#.|#......
.#..#.|#......
.#~~~~~#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...

Water pressure does not apply in this scenario. If another four squares of water are created, they will stay on the
right side of the barrier, and no water will reach the left side:

......+.......
......|.....#.
.#..#.|.....#.
.#..#~~#......
.#..#~~#......
.#~~~~~#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...

At this point, the top reservoir overflows. While water can reach the tiles above the surface of the water, it cannot
settle there, and so the next five squares of water settle like this:

......+.......
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
........|.....
....#...|.#...
....#...|.#...
....#~~~~~#...
....#######...

Note especially the leftmost |: the new squares of water can reach this tile, but cannot stop there. Instead,
eventually, they all fall to the right and settle in the reservoir below.

After 10 more squares of water, the bottom reservoir is also full:

......+.......
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
........|.....
....#~~~~~#...
....#~~~~~#...
....#~~~~~#...
....#######...

Finally, while there is nowhere left for the water to settle, it can reach a few more tiles before overflowing beyond
the bottom of the scanned data:

......+.......    (line not counted: above minimum y value)
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
...|||||||||..
...|#~~~~~#|..
...|#~~~~~#|..
...|#~~~~~#|..
...|#######|..
...|.......|..    (line not counted: below maximum y value)
...|.......|..    (line not counted: below maximum y value)
...|.......|..    (line not counted: below maximum y value)

How many tiles can be reached by the water? To prevent counting forever, ignore tiles with a y coordinate smaller
than the smallest y coordinate in your scan data or larger than the largest one. Any x coordinate is valid. In this
example, the lowest y coordinate given is 1, and the highest is 13, causing the water spring (in row 0) and the water
falling off the bottom of the render (in rows 14 through infinity) to be ignored.

So, in the example above, counting both water at rest (~) and other sand tiles the water can hypothetically reach (|),
the total number of tiles the water can reach is 57.

How many tiles can the water reach within the range of y values in your scan?
-}

parse ls =
  Set.unions $ catMaybes $ map (parseWith wallParser) ls

{-
x=377, y=393..396
y=105, x=454..476
-}

wallParser :: ReadP (Set.Set (Integer,Integer))
wallParser = xWallParser +++ yWallParser

xWallParser = do
  string "x="
  x <- intParser
  string ", y="
  y1 <- intParser
  string ".."
  y2 <- intParser
  eof
  return $ Set.fromList [(x,y) | y <- [y1..y2]]

yWallParser = do
  string "y="
  y <- intParser
  string ", x="
  x1 <- intParser
  string ".."
  x2 <- intParser
  eof
  return $ Set.fromList [(x,y) | x <- [x1..x2]]

xRange s =
  let xMin = minimum $ map (\(x,y) -> x) $ Set.toList s
      xMax = maximum $ map (\(x,y) -> x) $ Set.toList s
  in (xMin-1, xMax+1)

yRange s =
  let yMin = minimum $ map (\(x,y) -> y) $ Set.toList s
      yMax = maximum $ map (\(x,y) -> y) $ Set.toList s
  in (yMin, yMax)


d0 = lines "\
 \x=495, y=2..7\n\
 \y=7, x=495..501\n\
 \x=501, y=3..7\n\
 \x=498, y=2..4\n\
 \x=506, y=1..2\n\
 \x=498, y=10..13\n\
 \x=504, y=10..13\n\
 \y=13, x=498..504\n\
 \"


day17 :: [String] -> IO ()
day17 ls = do
  let layout = parse d0
      xs = xRange layout
      ys@(ymin,_) = yRange layout
      falling0 = Set.singleton (500,ymin)
      fixed0 = Set.empty
  putStrLn $ "x range is " ++ (show xs)
  putStrLn $ "y range is " ++ (show ys)

  showFlow xs ys layout falling0 fixed0


showFlow (xmin,xmax) (ymin,ymax) clay falling fixed = do
  forM_ [ymin..ymax] $ \y -> do
    forM_ [xmin..xmax] $ \x -> do
      putChar $ mapAt clay falling fixed x y
    putStrLn ""

mapAt clay falling fixed x y =
  if Set.member (x,y) clay then '#'
  else if Set.member (x,y) falling then '|'
  else if Set.member (x,y) fixed then '~'
  else '.'

{-
flow clay falling settled
-}




{-

-}


day17b :: [String] -> IO ()
day17b ls = do
  let result = ls
  putStrLn $ show result
