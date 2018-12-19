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
  Set.fromList $ catMaybes $ map (parseWith wallParser) ls

{-
x=377, y=393..396
y=105, x=454..476
-}

wallParser :: ReadP Wall
wallParser = xWallParser +++ yWallParser

data Wall = VWall Integer (Integer, Integer)
          | HWall (Integer, Integer) Integer
  deriving (Show, Eq, Ord)

vwall x y1 y2 = VWall x (y1, y2)
hwall x1 x2 y = HWall (x1, x2) y

xWallParser = do
  string "x="
  x <- intParser
  string ", y="
  y1 <- intParser
  string ".."
  y2 <- intParser
  eof
  return $ vwall x y1 y2

yWallParser = do
  string "y="
  y <- intParser
  string ", x="
  x1 <- intParser
  string ".."
  x2 <- intParser
  eof
  return $ hwall x1 x2 y

xRange s =
  let xMin = minimum $ map (\w -> case w of
                                  HWall (x,_) _ -> x
                                  VWall x _ -> x) $ Set.toList s
      xMax = maximum $ map (\w -> case w of
                                  HWall (_,x) _ -> x
                                  VWall x _ -> x) $ Set.toList s
  in (xMin-1, xMax+1)

yRange s =
  let yMin = minimum $ map (\w -> case w of
                                  HWall _ y -> y
                                  VWall _ (y,_) -> y) $ Set.toList s
      yMax = maximum $ map (\w -> case w of
                                  HWall _ y -> y
                                  VWall _ (_,y) -> y) $ Set.toList s
  in (yMin, yMax)


limitTo clay xmin xmax ymin ymax =
  let inBound = Set.filter (\w ->
                  case w of
                  HWall (x1, x2) y -> ymin <= y && y <= ymax && (xmin <= x2 || x1 <= xmax)
                  VWall x (y1, y2) -> xmin <= x && x <= xmax && (ymin <= y2 || y1 <= ymax)
                  ) clay
      trimmed = Set.map (\w ->
                  case w of
                  HWall (x1, x2) y -> HWall (xmin `max` x1, xmax `min` x2) y
                  VWall x (y1, y2) -> VWall x (ymin `max` y1, ymax `min` y2)
                  ) inBound
  in trimmed



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

d1 = lines "\
  \x=460, y=18..25\n\
  \x=470, y=12..22\n\
  \x=480, y=20..25\n\
  \x=490, y=14..19\n\
  \x=510, y=16..19\n\
  \x=520, y=10..22\n\
  \y=25, x=460..480\n\
  \y=22, x=470..520\n\
  \y=19, x=490..510\n\
  \"

d2 = lines "\
  \x=480, y=10..25\n\
  \x=520, y=10..25\n\
  \y=15, x=495..504\n\
  \x=495, y=12..15\n\
  \x=504, y=11..15\n\
  \y=20, x=490..497\n\
  \y=20, x=501..510\n\
  \x=490, y=17..20\n\
  \x=510, y=17..20\n\
  \"

d3 = lines "\
  \x=470, y=0..30\n\
  \y=30, x=470..530\n\
  \x=530, y=0..30\n\
  \x=480, y=20..25\n\
  \y=25, x=480..520\n\
  \x=520, y=20..25\n\
  \y=10, x=490..510\n\
  \x=500, y=13..17\n\
  \"

day17 :: [String] -> IO ()
day17 ls = do
  let clay = parse ls
      layout0' = layout (limitTo clay 480 520 0 39) 500
      layout0 = layout clay 500
  putStrLn $ "x range is " ++ (show $ xmin layout0) ++ "-" ++ (show $ xmax layout0)
  putStrLn $ "y range is " ++ (show $ ymin layout0) ++ "-" ++ (show $ ymax layout0)
  layout <- run layout0
  showFlow layout
  putStrLn "Calculating final score"
  putStrLn $ "Final score is: " ++ (show $ score layout)

  putStrLn "All done."

run layout0 = do
  putStrLn $ "Iteration " ++ (show $ Set.size $ activeSources layout0) ++ " " ++ (show $ Set.size $ rising layout0)
  let layout1 = flow layout0
      layout = rise layout1
  if layout == layout0
  then return layout
  else run layout

flow layout0 =
  if Set.null $ activeSources layout0
  then layout0
  else
    let source = Set.findMin $ activeSources layout0
    in  makeFall layout0 source

makeFall layout0 source@(x,y) =
  let as' = Set.delete source $ activeSources layout0
      l' = layout0 { activeSources=as' }
  {- Drop this source as far as it'll go -}
      (y', hits) = scanDown l' (x,y)
  in  if y' == y
      then l'
      else case hits of
      Offscreen -> l' { falls = Set.insert (vwall x y y') $ falls l' }
      Clay -> poolOrLake l' x y (y'-1)
      Lake -> poolOrLake l' x y (y'-1)
      Pool -> l' { falls = Set.insert (vwall x y y') $ falls l' }
      otherwise -> error $ "scanDown has hit: " ++ (show hits) ++ " at " ++ (show (x,y'))
  where
    scanDown layout (x,y) =
      case layout @@ (x,y) of
      Offscreen -> (y, Offscreen)
      Pool -> (y, Pool)
      Fall -> (y, Fall)
      Lake -> (y, Lake)
      Clay -> (y, Clay)
      Sand -> scanDown layout (x,y+1)

poolOrLake layout x y1 y2 =
  {- We're falling from (x,y1) onto (x,y2) which is above a poolable surface -}
  {- Scan left and right to locate edges to this pool -}
  let (x1, wall1) = scanOutward layout x (-1) y2
      (x2, wall2) = scanOutward layout x 1 y2
      l' = layout { falls = Set.insert (vwall x y1 y2) $ falls layout }
  in  case (wall1, wall2) of
      {- Spill off the left -}
      (False, True) -> l' { pools = Set.insert (hwall (x1+1) x2 y2) $ pools l'
                          , activeSources = Set.insert (x1, y2) $ activeSources l' }
      {- Spill off the right -}
      (True, False) -> l' { pools = Set.insert (hwall x1 (x2-1) y2) $ pools l'
                          , activeSources = Set.insert (x2, y2) $ activeSources l' }
      {- Spill off both sides -}
      (False, False) -> l' { pools = Set.insert (hwall (x1+1) (x2-1) y2) $ pools l'
                           , activeSources = Set.insert (x1, y2) $ Set.insert (x2,y2) $ activeSources l' }
      {- Form a lake and rise -}
      (True, True) ->
        if y2 > y1
        then poolOrLake l' { lakes = Set.insert (hwall x1 x2 y2) $ lakes l' } x y1 (y2-1)
        else l' { rising = Set.insert (hwall x1 x2 y1) $ rising l' } {- This lake will rise -}

{- Scan outward in a given direction. See if we hit a wall -}
scanOutward layout x dx y =
  case layout @@ (x, y+1) of
  Offscreen -> (x, False)
  Sand -> (x, False)
  Clay -> tryMove layout x dx y
  Lake -> tryMove layout x dx y
  Fall -> (x, False)
  otherwise -> error $ "Unknown option in scanOutward: " ++ (show $ layout @@ (x, y+1)) ++ " at " ++ (show (x,y+1))
  where
    tryMove layout x dx y =
      case layout @@ (x+dx, y) of
      Offscreen -> (x, False)
      Clay -> (x, True)
      Sand -> scanOutward layout (x+dx) dx y
      Lake -> scanOutward layout (x+dx) dx y
      Pool -> scanOutward layout (x+dx) dx y {- This pool subsumes the other -}
      Fall -> scanOutward layout (x+dx) dx y {- coast past another fall -}
      Source -> scanOutward layout (x+dx) dx y

rise layout =
  {- Handle any lakes which are rising -}
  if Set.null $ rising layout
  then layout
  else
    let lake = Set.findMin $ rising layout
    in  fill layout lake

fill layout lake@(HWall (x1, x2) y) =
  let l' = layout { lakes = Set.insert lake $ lakes layout
                  , rising = Set.delete lake $ rising layout }
      {- find flows that are falling into this puddle -}
      sources = filter (\x -> layout @@ (x,y-1) == Fall) [x1..x2]
  in  foldl (\l x -> poolOrLake l x (y-1) (y-1)) l' sources



data Layout = Layout { clay :: Set.Set Wall
                     , lakes :: Set.Set Wall
                     , pools :: Set.Set Wall  {- horizontal pools of flowing water -}
                     , falls :: Set.Set Wall  {- vertical falls of flowing water -}
                     , activeSources :: Set.Set (Integer, Integer)
                     , rising :: Set.Set Wall  {- rising horizontal puddles -}
                     , xmin :: Integer
                     , xmax :: Integer
                     , ymin :: Integer
                     , ymax :: Integer
                     }
  deriving (Eq)


layout c x =
  let (xmin,xmax) = xRange c
      (ymin,ymax) = yRange c
  in  Layout { clay = c
             , lakes = Set.empty
             , pools = Set.empty
             , falls = Set.empty
             , activeSources = Set.singleton (x,ymin)
             , rising = Set.empty
             , xmin = xmin, xmax = xmax
             , ymin = ymin, ymax = ymax
             }

data Cell = Clay | Lake | Pool | Fall | Source | Sand | Offscreen
  deriving (Eq, Show)

showFlow layout = do
  forM_ [ymin layout..ymax layout] $ \y -> do
    forM_ [xmin layout..xmax layout] $ \x -> do
      putChar $ charFor $ layout @@ (x,y)
    putStrLn ""
  where
    charFor Clay = '#'
    charFor Lake = '~'
    charFor Pool = '-'
    charFor Fall = '|'
    charFor Source = '+'
    charFor Sand = '.'

infix 8 @@

(@@) :: Layout -> (Integer, Integer) -> Cell
scene @@ (x, y) =
  if y > ymax scene || y < ymin scene || x < xmin scene || x > xmax scene then Offscreen
  else if member (x,y) $ clay scene then Clay
  else if Set.member (x,y) $ activeSources scene then Source
  else if member (x,y) $ lakes scene then Lake
  else if member (x,y) $ pools scene then Pool
  else if member (x,y) $ falls scene then Fall
  else Sand
  where
    member :: (Integer,Integer) -> (Set.Set Wall) -> Bool
    member (x, y) s = any (\w -> case w of
                                 VWall x' (y1,y2) -> x == x' && y1 <= y && y <= y2
                                 HWall (x1,x2) y' -> y == y' && x1 <= x && x <= x2) $ Set.toList s

score layout =
  let water = Set.unions [ lakes layout
                         , falls layout
                         , pools layout
                         ]
      trimmed = limitTo water (xmin layout) (xmax layout) (ymin layout) (ymax layout)
  in  Set.size $ foldMap xys trimmed
  where
    xys wall = case wall of
               HWall (x1, x2) y -> Set.fromList [(x,y) | x <- [x1..x2]]
               VWall x (y1, y2) -> Set.fromList [(x,y) | y <- [y1..y2]]


{-
After a very long time, the water spring will run dry. How much water will be retained?

In the example above, water that won't eventually drain out is shown as ~, a total of 29 tiles.

How many water tiles are left after the water spring stops producing water and all remaining water not at rest has
drained?
-}


day17b :: [String] -> IO ()
day17b ls = do
  let clay = parse ls
      layout0 = layout clay 500
  layout <- run layout0
  showFlow layout
  putStrLn "Calculating final score"
  putStrLn $ "Final score is: " ++ (show $ score layout)
  putStrLn $ "Final permanent lake score is: " ++ (show $ score2 layout)


score2 layout =
  let trimmed = limitTo (lakes layout) (xmin layout) (xmax layout) (ymin layout) (ymax layout)
  in  Set.size $ foldMap xys trimmed
  where
    xys wall = case wall of
               HWall (x1, x2) y -> Set.fromList [(x,y) | x <- [x1..x2]]
               VWall x (y1, y2) -> Set.fromList [(x,y) | y <- [y1..y2]]
