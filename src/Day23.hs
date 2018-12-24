module Day23
    ( day23
    , day23b
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
import Data.Algorithm.MaximalCliques as BK


{-
Using your torch to search the darkness of the rocky cavern, you finally locate the man's friend: a small reindeer.

You're not sure how it got so far in this cave. It looks sick - too sick to walk - and too heavy for you to carry all the way back. Sleighs won't be invented for another 1500 years, of course.

The only option is experimental emergency teleportation.

You hit the "experimental emergency teleportation" button on the device and push I accept the risk on no fewer than 18 different warning messages. Immediately, the device deploys hundreds of tiny nanobots which fly around the cavern, apparently assembling themselves into a very specific formation. The device lists the X,Y,Z position (pos) for each nanobot as well as its signal radius (r) on its tiny screen (your puzzle input).

Each nanobot can transmit signals to any integer coordinate which is a distance away from it less than or equal to its signal radius (as measured by Manhattan distance). Coordinates a distance away of less than or equal to a nanobot's signal radius are said to be in range of that nanobot.

Before you start the teleportation process, you should determine which nanobot is the strongest (that is, which has the largest signal radius) and then, for that nanobot, the total number of nanobots that are in range of it, including itself.

For example, given the following nanobots:

pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1

The strongest nanobot is the first one (position 0,0,0) because its signal radius, 4 is the largest. Using that nanobot's location and signal radius, the following nanobots are in or out of range:

    The nanobot at 0,0,0 is distance 0 away, and so it is in range.
    The nanobot at 1,0,0 is distance 1 away, and so it is in range.
    The nanobot at 4,0,0 is distance 4 away, and so it is in range.
    The nanobot at 0,2,0 is distance 2 away, and so it is in range.
    The nanobot at 0,5,0 is distance 5 away, and so it is not in range.
    The nanobot at 0,0,3 is distance 3 away, and so it is in range.
    The nanobot at 1,1,1 is distance 3 away, and so it is in range.
    The nanobot at 1,1,2 is distance 4 away, and so it is in range.
    The nanobot at 1,3,1 is distance 5 away, and so it is not in range.

In this example, in total, 7 nanobots are in range of the nanobot with the largest signal radius.

Find the nanobot with the largest signal radius. How many nanobots are in range of its signals?
-}


parse ls = catMaybes $ map (parseWith lineParser) ls

lineParser = do
  string "pos=<"
  x <- intParser
  char ','
  y <- intParser
  char ','
  z <- intParser
  string ">, r="
  r <- intParser
  eof
  return (x, y, z, r)


day23 :: [String] -> IO ()
day23 ls = do
  let bots = parse ls
      sorted = List.sortBy (\(_,_,_,r1) (_,_,_,r2) -> compare r2 r1) bots
      strongest@(x,y,z,r) = head sorted
      nearby = filter (\bot -> dist strongest bot <= r) bots
  putStrLn $ show sorted
  putStrLn $ show nearby
  putStrLn $ show $ length nearby

dist (x1, y1, z1, _) (x2, y2, z2, _) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

{-
Now, you just need to figure out where to position yourself so that you're actually teleported when the nanobots
activate.

To increase the probability of success, you need to find the coordinate which puts you in range of the largest
number of nanobots. If there are multiple, choose one closest to your position (0,0,0, measured by manhattan
distance).

For example, given the following nanobot formation:

pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5

Many coordinates are in range of some of the nanobots in this formation. However, only the coordinate 12,12,12 is
in range of the most nanobots: it is in range of the first five, but is not in range of the nanobot at 10,10,10.
(All other coordinates are in range of fewer than five nanobots.) This coordinate's distance from 0,0,0 is 36.

Find the coordinates that are in range of the largest number of nanobots. What is the shortest manhattan distance
between any of those points and 0,0,0?
-}


day23b :: [String] -> IO ()
day23b ls = do
  let bots = parse ls
      sorted = List.sortBy (\(_,_,_,r1) (_,_,_,r2) -> compare r2 r1) bots
      strongest@(x,y,z,r) = head sorted
      nearby = filter (\bot -> dist strongest bot <= r) bots
  putStrLn $ show sorted
  putStrLn $ show $ length sorted
  putStrLn $ show nearby
  putStrLn $ show $ length nearby
  {- Right. For any set of bots, if they pairwise overlap then there's a general intersection
     (This is a result from Taxicab geometry -}
  let (maxClique:_) = BK.getMaximalCliques touching sorted
  putStrLn $ show $ length maxClique
  putStrLn $ show $ maximum [dist (0,0,0,0) p - r | p@(_,_,_,r) <- maxClique]

touching p1@(_,_,_,r1) p2@(_,_,_,r2) = dist p1 p2 <= r1 + r2