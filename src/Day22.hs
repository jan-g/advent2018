module Day22
    ( day22
    , day22b
    , demo
    , showTerrain2
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
import qualified Data.Heap as Heap


{-
This is it, your final stop: the year -483. It's snowing and dark outside; the only light you can see is coming from a small cottage in the distance. You make your way there and knock on the door.

A portly man with a large, white beard answers the door and invites you inside. For someone living near the North Pole in -483, he must not get many visitors, but he doesn't act surprised to see you. Instead, he offers you some milk and cookies.

After talking for a while, he asks a favor of you. His friend hasn't come back in a few hours, and he's not sure where he is. Scanning the region briefly, you discover one life signal in a cave system nearby; his friend must have taken shelter there. The man asks if you can go there to retrieve his friend.

The cave is divided into square regions which are either dominantly rocky, narrow, or wet (called its type). Each region occupies exactly one coordinate in X,Y format where X and Y are integers and zero or greater. (Adjacent regions can be the same type.)

The scan (your puzzle input) is not very detailed: it only reveals the depth of the cave system and the coordinates of the target. However, it does not reveal the type of each region. The mouth of the cave is at 0,0.

The man explains that due to the unusual geology in the area, there is a method to determine any region's type based on its erosion level. The erosion level of a region can be determined from its geologic index. The geologic index can be determined using the first rule that applies from the list below:

    The region at 0,0 (the mouth of the cave) has a geologic index of 0.
    The region at the coordinates of the target has a geologic index of 0.
    If the region's Y coordinate is 0, the geologic index is its X coordinate times 16807.
    If the region's X coordinate is 0, the geologic index is its Y coordinate times 48271.
    Otherwise, the region's geologic index is the result of multiplying the erosion levels of the regions at X-1,Y and X,Y-1.

A region's erosion level is its geologic index plus the cave system's depth, all modulo 20183. Then:

    If the erosion level modulo 3 is 0, the region's type is rocky.
    If the erosion level modulo 3 is 1, the region's type is wet.
    If the erosion level modulo 3 is 2, the region's type is narrow.

For example, suppose the cave system's depth is 510 and the target's coordinates are 10,10. Using % to represent the modulo operator, the cavern would look as follows:

    At 0,0, the geologic index is 0. The erosion level is (0 + 510) % 20183 = 510. The type is 510 % 3 = 0, rocky.
    At 1,0, because the Y coordinate is 0, the geologic index is 1 * 16807 = 16807. The erosion level is (16807 + 510) % 20183 = 17317. The type is 17317 % 3 = 1, wet.
    At 0,1, because the X coordinate is 0, the geologic index is 1 * 48271 = 48271. The erosion level is (48271 + 510) % 20183 = 8415. The type is 8415 % 3 = 0, rocky.
    At 1,1, neither coordinate is 0 and it is not the coordinate of the target, so the geologic index is the erosion level of 0,1 (8415) times the erosion level of 1,0 (17317), 8415 * 17317 = 145722555. The erosion level is (145722555 + 510) % 20183 = 1805. The type is 1805 % 3 = 2, narrow.
    At 10,10, because they are the target's coordinates, the geologic index is 0. The erosion level is (0 + 510) % 20183 = 510. The type is 510 % 3 = 0, rocky.

Drawing this same cave system with rocky as ., wet as =, narrow as |, the mouth as M, the target as T, with 0,0 in the top-left corner, X increasing to the right, and Y increasing downward, the top-left corner of the map looks like this:

M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Before you go in, you should determine the risk level of the area. For the the rectangle that has a top-left corner of region 0,0 and a bottom-right corner of the region containing the target, add up the risk level of each individual region: 0 for rocky regions, 1 for wet regions, and 2 for narrow regions.

In the cave system above, because the mouth is at 0,0 and the target is at 10,10, adding up the risk level of all regions with an X coordinate from 0 to 10 and a Y coordinate from 0 to 10, this total is 114.

What is the total risk level for the smallest rectangle that includes 0,0 and the target's coordinates?
-}

parse :: [String] -> (Integer, (Integer, Integer))
parse ls =
  let Just depth = parseWith depthParser $ ls !! 0
      Just target = parseWith targetParser $ ls !! 1
  in  (depth, target)

depthParser = do
  string "depth: "
  d <- intParser
  eof
  return d

targetParser = do
  string "target: "
  x <- intParser
  char ','
  y <- intParser
  eof
  return (x, y)

day22 :: [String] -> IO ()
day22 ls = do
  let (depth, target@(x,y)) = parse ls
      result = eMap depth target target :: Array.Array (Integer, Integer) Integer
  putStrLn $ show (depth, target)
  putStrLn $ show $ sum $ map (`mod` 3) $ Array.elems result
  putStrLn $ show $ (terrain result) ! target
  let r2 = eMap depth target (x+5,y+5)
  putStrLn $ show $ (terrain r2) ! target

xMul = 16807
yMul = 48271
eMod = 20183


eMap depth (tx, ty) (xmax, ymax) =
  let erosion0 = [(x * xMul + depth) `mod` eMod | x <- [0..xmax]]
      e1 = foldl (\es@(above:_) y ->
                              let erosion = line y [(yMul * y + depth) `mod` eMod] [1..xmax] $ tail above
                              in (erosion:es)) ([erosion0]) [1..ymax]
      erosion = reverse e1
  in Array.listArray  ((0,0), (xmax, ymax)) $ concat $ List.transpose erosion
  -- in Array.array ((0,0), (xmax, ymax)) [((x,y),e) | (y,l) <- zip [0..] erosion, (x,e) <- zip [0..] l]
  where
    line :: Integer -> [Integer] -> [Integer] -> [Integer] -> [Integer]
    line _ acc [] [] = reverse acc
    line y es@(left:_) (x:xs) (above:aboves) =
      let g = if y == ty && x == tx
              then 0
              else left * above
          e = (g + depth) `mod` eMod
      in  line y (e:es) xs aboves


{- Here's a sneaky doubly-infinite definition of the map -}
eMap2 depth (tx, ty) =
  map (\(y,row) -> map (\(x, cell) -> cell) row) lines
  where
    y0 = [(x, (x * xMul + depth) `mod` eMod) | x <- [0..]]
    lines = ((0, y0):(map nextLine lines))
    nextLine (yup, above) = (y, row)
      where
        y = yup + 1
        x0 = (yMul * y + depth) `mod` eMod
        row = (0, x0):(map nextCell ((tail above) `zip` row))
        nextCell ((_, up), (xl, left)) = (x, cell)
          where
            x = xl + 1
            cell = (g + depth) `mod` eMod
              where
                g | y == ty && x == tx = 0
                  | otherwise = up * left


{-
Okay, it's time to go rescue the man's friend.

As you leave, he hands you some tools: a torch and some climbing gear. You can't equip both tools at once, but you
can choose to use neither.

Tools can only be used in certain regions:

    In rocky regions, you can use the climbing gear or the torch. You cannot use neither (you'll likely slip and
      fall).
    In wet regions, you can use the climbing gear or neither tool. You cannot use the torch (if it gets wet, you
      won't have a light source).
    In narrow regions, you can use the torch or neither tool. You cannot use the climbing gear (it's too bulky
      to fit).

You start at 0,0 (the mouth of the cave) with the torch equipped and must reach the target coordinates as quickly
as possible. The regions with negative X or Y are solid rock and cannot be traversed. The fastest route might
involve entering regions beyond the X or Y coordinate of the target.

You can move to an adjacent region (up, down, left, or right; never diagonally) if your currently equipped tool
allows you to enter that region. Moving to an adjacent region takes one minute. (For example, if you have the torch
equipped, you can move between rocky and narrow regions, but cannot enter wet regions.)

You can change your currently equipped tool or put both away if your new equipment would be valid for your current
region. Switching to using the climbing gear, torch, or neither always takes seven minutes, regardless of which
tools you start with. (For example, if you are in a rocky region, you can switch from the torch to the climbing
gear, but you cannot switch to neither.)

Finally, once you reach the target, you need the torch equipped before you can find him in the dark. The target is
always in a rocky region, so if you arrive there with climbing gear equipped, you will need to spend seven minutes
switching to your torch.

For example, using the same cave system as above, starting in the top left corner (0,0) and moving to the bottom
right corner (the target, 10,10) as quickly as possible, one possible route is as follows, with your current
position marked X:

Initially:
X=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Down:
M=.|=.|.|=.|=|=.
X|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right:
M=.|=.|.|=.|=|=.
.X=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Switch from using the torch to neither tool:
M=.|=.|.|=.|=|=.
.X=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right 3:
M=.|=.|.|=.|=|=.
.|=|X|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Switch from using neither tool to the climbing gear:
M=.|=.|.|=.|=|=.
.|=|X|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Down 7:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..X==..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..=X=..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Down 3:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||.X.|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||..X|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Down:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.X..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right 4:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=X||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Up 2:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===X===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Switch from using the climbing gear to the torch:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===X===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

This is tied with other routes as the fastest way to reach the target: 45 minutes. In it, 21 minutes are spent
switching tools (three times, seven minutes each) and the remaining 24 minutes are spent moving.

What is the fewest number of minutes you can take to reach the target?
-}

data Tool = Torch | Climbing | None
  deriving (Ord, Eq, Show)

data Terrain = Rocky | Wet | Narrow
  deriving (Show)

type TerrainMap = Array.Array (Integer, Integer) Terrain

terrain :: Array.Array (Integer,Integer) Integer -> TerrainMap
terrain erosion =
  (Array.listArray (Array.bounds erosion) $ map toTerrain $ Array.elems erosion)
  where
    toTerrain e = case e `mod` 3 of
      0 -> Rocky
      1 -> Wet
      2 -> Narrow

validTool Rocky Torch = True
validTool Rocky Climbing = True
validTool Wet Climbing = True
validTool Wet None = True
validTool Narrow Torch = True
validTool Narrow None = True
validTool _ _ = False

otherTool Rocky Torch = Climbing
otherTool Rocky Climbing = Torch
otherTool Wet Climbing = None
otherTool Wet None = Climbing
otherTool Narrow Torch = None
otherTool Narrow None = Torch

day22b :: [String] -> IO ()
day22b ls = do
  let (depth, (tx,ty)) = parse ls -- (510, (10, 10))
      dMax = (tx `max` ty) * 7
      (maxX, maxY) = (dMax, dMax) -- (tx * 2, ty + 30)
      erosion = eMap depth (tx, ty) (maxX, maxY)
      t = terrain erosion
  putStrLn $ show (depth, (tx,ty))
  putStrLn $ "Origin is in a region that is " ++ (show $ t ! (0,0))
  putStrLn $ "Target is in a region that is " ++ (show $ t ! (tx,ty))
  time <- bfs t (tx, ty) (maxX, maxY) (Set.empty) (Heap.singleton (0, (0, 0), Torch)) (-1)
  putStrLn $ show time

bfs :: TerrainMap ->
       (Integer, Integer) ->  -- the target location
       (Integer, Integer) ->  -- maximum bounds of search
       Set.Set (Integer,Integer,Tool) ->  -- the earliest time to reach (x,y) carrying a particular tool
       Heap.MinHeap (Integer, (Integer, Integer), Tool) ->  -- a priority queue of where we might reach next
       Integer -> -- last time displayed
         IO Integer -- time of reaching
bfs layout (tx,ty) (mx,my) reached moveHeap lastT = do
  let Just ((t, (x, y), curTool), h0) = Heap.view moveHeap
  if Set.member (x, y, curTool) reached
  then bfs layout (tx, ty) (mx,my) reached h0 lastT
  else do
    let reached' = Set.insert (x, y, curTool) reached
    if x == tx && y == ty && curTool == Torch
    then return t
    else do
        lastT' <- if t /= lastT && x == tx
                  then do
                    putStrLn $ (show (t, x,y, curTool))
                    return t
                  else do
                    return lastT
        let newTool = otherTool (layout ! (x,y)) curTool
            {- change tools lazily; there's no point swapping needlessly.
               It massively explodes the search space. -}
            h1 = if canExplore (x-1, y) curTool
                 then Heap.insert (t+1, (x-1,y), curTool) h0
                 else if canExplore (x-1,y) newTool
                 then Heap.insert (t+8, (x-1,y), newTool) h0
                 else h0
            h2 = if canExplore (x+1, y) curTool
                 then Heap.insert (t+1, (x+1,y), curTool) h1
                 else if canExplore (x+1,y) newTool
                 then Heap.insert (t+8, (x+1,y), newTool) h1
                 else h1
            h3 = if canExplore (x, y-1) curTool
                 then Heap.insert (t+1, (x,y-1), curTool) h2
                 else if canExplore (x,y-1) newTool
                 then Heap.insert (t+8, (x,y-1), newTool) h2
                 else h2
            h4 = if canExplore (x, y+1) curTool
                 then Heap.insert (t+1, (x,y+1), curTool) h3
                 else if canExplore (x,y+1) newTool
                 then Heap.insert (t+8, (x,y+1), newTool) h3
                 else h3
        h5 <- if x == tx && y == ty
                  then do
                    putStrLn $ "Got to " ++ (show (x,y)) ++ " with " ++ (show curTool) ++ " and switchable to " ++ (show newTool)
                    return $ Heap.insert (t+7, (x,y), newTool) h4
                  else return h4
        bfs layout (tx,ty) (mx,my) reached' h5 lastT'
  where
    canExplore (x', y') tool
      | x' < 0 || x' > mx = False    -- don't explore to the left
      | y' < 0 || y' > my = False    -- don't explore above the start
      | Set.member (x', y', tool) reached = False   -- we've already tried that
      | otherwise = validTool (layout ! (x', y')) tool


showTerrain t =
  let ((x1,y1),(x2,y2)) = Array.bounds t
  in  (unlines [[case t ! (x,y) of
                  Rocky -> '.'
                  Wet -> '='
                  Narrow -> '|' | x <- [x1..x2]] | y <- [y1..y2]])
      ++ "x1,y1=" ++ (show (x1,y1)) ++ "; x2,y2=" ++ (show (x2,y2))


showTerrain2 depth tx ty mx my =
  unlines [[case toTerrain $ (eMap2 depth (tx, ty)) !! (fromIntegral y) !! (fromIntegral x) of
                  Rocky -> '.'
                  Wet -> '='
                  Narrow -> '|' | x <- [0..mx]] | y <- [0..my]]
  where
    toTerrain e = case e `mod` 3 of
      0 -> Rocky
      1 -> Wet
      2 -> Narrow

demo = do
  let (depth, (tx,ty)) = (510, (10, 10))
  putStrLn $ showTerrain2 depth tx ty (tx + 5) (ty + 5)
