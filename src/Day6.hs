module Day6
    ( day6
    , day6b
    ) where

import Text.ParserCombinators.ReadP
import Data.Maybe
import Lib (intParser)


{-
The device on your wrist beeps several times, and once again you feel like you're falling.

"Situation critical," the device announces. "Destination indeterminate. Chronal interference detected. Please specify
new target coordinates."

The device then produces a list of coordinates (your puzzle input). Are they places it thinks are safe or dangerous? It
recommends you check manual page 729. The Elves did not give you a manual.

If they're dangerous, maybe you can minimize the danger by finding the coordinate that gives the largest distance from
the other points.

Using only the Manhattan distance, determine the area around each coordinate by counting the number of integer X,Y
locations that are closest to that coordinate (and aren't tied in distance to any other coordinate).

Your goal is to find the size of the largest area that isn't infinite. For example, consider the following list of
coordinates:

1, 1
1, 6
8, 3
3, 4
5, 5
8, 9

If we name these coordinates A through F, we can draw them on a grid, putting 0,0 at the top left:

..........
.A........
..........
........C.
...D......
.....E....
.B........
..........
..........
........F.

This view is partial - the actual grid extends infinitely in all directions. Using the Manhattan distance, each
location's closest coordinate can be determined, shown here in lowercase:

aaaaa.cccc
aAaaa.cccc
aaaddecccc
aadddeccCc
..dDdeeccc
bb.deEeecc
bBb.eeee..
bbb.eeefff
bbb.eeffff
bbb.ffffFf

Locations shown as . are equally far from two or more coordinates, and so they don't count as being closest to any.

In this example, the areas of coordinates A, B, C, and F are infinite - while not shown here, their areas extend
forever outside the visible grid. However, the areas of coordinates D and E are finite: D is closest to 9 locations,
and E is closest to 17 (both including the coordinate's location itself). Therefore, in this example, the size of the
largest area is 17.

What is the size of the largest area that isn't infinite?
-}


parseLine :: String -> Maybe (Integer, Integer)
parseLine s =
  case readP_to_S lineParser s of
    [] -> Nothing
    [(e, "")] -> Just e

lineParser :: ReadP (Integer, Integer)
lineParser = do
  x <- intParser
  char ','
  skipSpaces
  y <- intParser
  eof
  return (x, y)





day6 :: [String] -> IO ()
day6 ls = do
  let coords = catMaybes $ map parseLine ls
  {- The approach here: we define a border that's 'far enough' outside the extremes of the given points
     in all the cardinal directions.

     We then compute the manhattan distances to each coordinate within that region.

     If any coord is the closest origin to an edge point, we reject that since its claimed region
     must be infinite.

     (Proof: if coordinate X claims a border point on, say, the left edge, then all coords are at a distance one
     greater to the point immediately to the left of that. Thus, coord X must be the closest to that point also,
     and so on.)

     For the remaining points, we simply take the smallest region.

     What's 'far enough'?

     If we were to take a diagonal lines outward from the corners of the bounding rectangle of these coords,
     their meeting-point is definitely 'far enough'.
  -}
      leftX = minimum [x | (x, _) <- coords]
      rightX = maximum [x | (x, _) <- coords]
      topY = minimum [y | (_, y) <- coords]
      bottomY = maximum [y | (_, y) <- coords]
      minX = leftX - (bottomY - topY)
      maxX = rightX + (bottomY - topY)
      minY = topY - (rightX - leftX)
      maxY = bottomY + (rightX - leftX)

      sizeMap = [size | origin <- coords,
                        let size = computeRegion minX maxX minY maxY origin coords ]
      largest = maximum $ catMaybes sizeMap

  putStrLn ("day6 " ++ (show largest))


{- This isn't very efficient, but it works -}
computeRegion minX maxX minY maxY origin coords =
  let zone = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY],
                       all (\ other -> dist origin (x, y) < dist other (x, y))
                           [other | other <- coords, other /= origin]]
  in if any (\ (x, y) -> x == minX || x == maxX || y == minY || y == maxY) zone
  then Nothing
  else Just $ length zone

dist (x, y) (x', y') = (abs (x - x')) + (abs (y - y'))

{-
On the other hand, if the coordinates are safe, maybe the best you can do is try to find a region near as many coordinates as possible.

For example, suppose you want the sum of the Manhattan distance to all of the coordinates to be less than 32. For each location, add up the distances to all of the given coordinates; if the total of those distances is less than 32, that location is within the desired region. Using the same coordinates as above, the resulting region looks like this:

..........
.A........
..........
...###..C.
..#D###...
..###E#...
.B.###....
..........
..........
........F.

In particular, consider the highlighted location 4,3 located at the top middle of the region. Its calculation is as follows, where abs() is the absolute value function:

    Distance to coordinate A: abs(4-1) + abs(3-1) =  5
    Distance to coordinate B: abs(4-1) + abs(3-6) =  6
    Distance to coordinate C: abs(4-8) + abs(3-3) =  4
    Distance to coordinate D: abs(4-3) + abs(3-4) =  2
    Distance to coordinate E: abs(4-5) + abs(3-5) =  3
    Distance to coordinate F: abs(4-8) + abs(3-9) = 10
    Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

Because the total distance to all coordinates (30) is less than 32, the location is within the region.

This region, which also includes coordinates D and E, has a total size of 16.

Your actual region will need to be much larger than this example, though, instead including all locations with a total distance of less than 10000.

What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?
-}


day6b :: [String] -> IO ()
day6b ls = do
  let coords = catMaybes $ map parseLine ls
      maxDist = 10000
  {-
  This time, we're going to be smarter.
  Firstly, assuming the best case for y-coords, let's work out the maximum bounds of the x region that is
  within maxDist units of all coordinates (in total).
  We do this by beginning with a large range, then reducing its bounds for each coordinate in turn until
  they've all been processed.
  That'll give us a [minX, maxX] range - or Nothing, if it's not possible to do this.

  Then we scan across that range.

  Given a particular x coordinate that we're scanning, we tot up the total x distance to that line. Subtracting
  this from maxDist gives us the maximal possible range for y coordinates. We use this to repeat the
  boundary-reduction process for the y direction.

  If we end up with a valid y range, we add its size in cells to a running total.
  -}
      (x, y) = head coords
      xs = map (\(x, _) -> x) coords
      ys = map (\(_, y) -> y) coords
      (minX, maxX) = (x - maxDist, x + maxDist)
      xDists = bracket maxDist minX maxX xs
      area = sum [length slice
                 |(dist, _) <- xDists,
                  let maxDist' = maxDist - dist
                      (minY, maxY) = (y - maxDist', y + maxDist')
                      slice = bracket maxDist' minY maxY ys]
  putStrLn $ show area

bracket maxDist minC maxC cs =
  [(dist, c) | c <- [minC .. maxC],
               let dist = sum [abs(c - c') | c' <- cs],
               maxDist > dist]