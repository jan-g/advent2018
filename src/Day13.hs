module Day13
    ( day13
    , day13b
    ) where

import Lib
import Text.ParserCombinators.ReadP
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Array ((!))


{-
A crop of this size requires significant logistics to transport produce, soil, fertilizer, and so on. The Elves are
very busy pushing things around in carts on some kind of rudimentary system of tracks they've come up with.

Seeing as how cart-and-track systems don't appear in recorded history for another 1000 years, the Elves seem to be
making this up as they go along. They haven't even figured out how to avoid collisions yet.

You map out the tracks (your puzzle input) and see where you can help.

Tracks consist of straight paths (| and -), curves (/ and \), and intersections (+). Curves connect exactly two
perpendicular pieces of track; for example, this is a closed loop:

/----\
|    |
|    |
\----/

Intersections occur when two perpendicular paths cross. At an intersection, a cart is capable of turning left,
turning right, or continuing straight. Here are two loops connected by two intersections:

/-----\
|     |
|  /--+--\
|  |  |  |
\--+--/  |
   |     |
   \-----/

Several carts are also on the tracks. Carts always face either up (^), down (v), left (<), or right (>). (On your
initial map, the track under each cart is a straight path matching the direction the cart is facing.)

Each time a cart has the option to turn (by arriving at any intersection), it turns left the first time, goes straight
the second time, turns right the third time, and then repeats those directions starting again with left the fourth
time, straight the fifth time, and so on. This process is independent of the particular intersection at which the cart
has arrived - that is, the cart has no per-intersection memory.

Carts all move at the same speed; they take turns moving a single step at a time. They do this based on their current
location: carts on the top row move first (acting from left to right), then carts on the second row move (again from
left to right), then carts on the third row, and so on. Once each cart has moved one step, the process repeats; each
of these loops is called a tick.

For example, suppose there are two carts on a straight track:

|  |  |  |  |
v  |  |  |  |
|  v  v  |  |
|  |  |  v  X
|  |  ^  ^  |
^  ^  |  |  |
|  |  |  |  |

First, the top cart moves. It is facing down (v), so it moves down one square. Second, the bottom cart moves. It is
facing up (^), so it moves up one square. Because all carts have moved, the first tick ends. Then, the process repeats,
starting with the first cart. The first cart moves down, then the second cart moves up - right into the first cart,
colliding with it! (The location of the crash is marked with an X.) This ends the second and last tick.

Here is a longer example:

/->-\
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/

/-->\
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \->--/
  \------/

/---v
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+>-/
  \------/

/---\
|   v  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+->/
  \------/

/---\
|   |  /----\
| /->--+-\  |
| | |  | |  |
\-+-/  \-+--^
  \------/

/---\
|   |  /----\
| /-+>-+-\  |
| | |  | |  ^
\-+-/  \-+--/
  \------/

/---\
|   |  /----\
| /-+->+-\  ^
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /----<
| /-+-->-\  |
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /---<\
| /-+--+>\  |
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /--<-\
| /-+--+-v  |
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /-<--\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/

/---\
|   |  /<---\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-<--/
  \------/

/---\
|   |  v----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \<+--/
  \------/

/---\
|   |  /----\
| /-+--v-\  |
| | |  | |  |
\-+-/  ^-+--/
  \------/

/---\
|   |  /----\
| /-+--+-\  |
| | |  X |  |
\-+-/  \-+--/
  \------/

After following their respective paths for a while, the carts eventually crash. To help prevent crashes, you'd like to
know the location of the first crash. Locations are given in X,Y coordinates, where the furthest left column is X=0 and
the furthest top row is Y=0:

           111
 0123456789012
0/---\
1|   |  /----\
2| /-+--+-\  |
3| | |  X |  |
4\-+-/  \-+--/
5  \------/

In this example, the location of the first crash is 7,3.
-}

data Track = Track { layout :: Map.Map (Integer, Integer) Char
                   }
  deriving (Eq)

data Turn = TLeft | TStraight | TRight
  deriving (Ord, Eq)

data Cart = Cart { pos :: (Integer, Integer)
                 , dir :: (Integer, Integer)
                 , turns :: [Turn]
                 }
  deriving (Eq)

instance Show Cart where
  show Cart { pos=p, dir=d } =
    (case d of
      (0, 1) -> "v"
      (0, -1) -> "^"
      (-1, 0) -> "<"
      (1, 0) -> ">") ++ (show p)

instance Ord Cart where
  compare Cart { pos=(x1,y1) } Cart { pos=(x2,y2) } = compare (y1,x1) (y2,x2)

turnOrder :: [Turn]
turnOrder = (TLeft:TStraight:TRight:turnOrder)

parse :: [String] -> (Track, Set.Set Cart)
parse ls =
  let layout0 = Map.empty :: Map.Map (Integer, Integer) Char
      carts0 = Set.empty :: Set.Set Cart
      (layoutN, cartsN) = foldl parseLine (layout0, carts0) $ zip [0..] ls
  in  (Track { layout=layoutN }, cartsN)
  where
    parseLine (layout0, carts0) (y, line) = foldl (parseCol y) (layout0, carts0) $ zip [0..] line
    parseCol y (layout0, carts0) (x, c) =
      case c of
      ' ' -> (layout0, carts0)
      '-' -> let layout1 = Map.insert (x, y) c layout0
             in  (layout1, carts0)
      '|' -> let layout1 = Map.insert (x, y) c layout0
             in  (layout1, carts0)
      '\\' -> let layout1 = Map.insert (x, y) c layout0
              in  (layout1, carts0)
      '/' -> let layout1 = Map.insert (x, y) c layout0
             in  (layout1, carts0)
      '+' -> let layout1 = Map.insert (x, y) c layout0
             in  (layout1, carts0)
      'v' -> let layout1 = Map.insert (x, y) '|' layout0
                 carts1 = Set.insert Cart { pos=(x,y), dir=(0,1), turns=turnOrder } carts0
             in  (layout1, carts1)
      '^' -> let layout1 = Map.insert (x, y) '|' layout0
                 carts1 = Set.insert Cart { pos=(x,y), dir=(0,-1), turns=turnOrder } carts0
             in  (layout1, carts1)
      '<' -> let layout1 = Map.insert (x, y) '-' layout0
                 carts1 = Set.insert Cart { pos=(x,y), dir=(-1,0), turns=turnOrder } carts0
             in  (layout1, carts1)
      '>' -> let layout1 = Map.insert (x, y) '-' layout0
                 carts1 = Set.insert Cart { pos=(x,y), dir=(1,0), turns=turnOrder } carts0
             in  (layout1, carts1)

turn TLeft (dx, dy) = (dy, -dx)
turn TStraight (dx, dy) = (dx, dy)
turn TRight (dx, dy) = (-dy, dx)

moveCart track Cart { pos=(x, y), dir=(dx, dy), turns=turns@(t0:ts) } =
  let pos' = (x + dx, y + dy)
      c = (layout track) Map.! pos'
      (dir', turns') = case c of
        '|' -> ((dx, dy), turns)
        '-' -> ((dx, dy), turns)
        '/' -> ((-dy, -dx), turns)
        '\\' -> ((dy, dx), turns)
        '+' -> ((turn t0 (dx, dy)), ts)
  in Cart { pos=pos', dir=dir', turns=turns' }

day13 :: [String] -> IO ()
day13 ls = do
  let (layout, cars) = parse ls
  result <- runUntilCollided layout 0 cars
  putStrLn $ show result

runUntilCollided track iter cars = do
  runUntilCollided0 track iter (Set.toAscList cars) Set.empty
  where
    runUntilCollided0 track iter [] cars = do
      putStrLn ("Iteration " ++ (show iter) ++ " " ++ (show cars))
      runUntilCollided0 track (iter + 1) (Set.toAscList cars) Set.empty
    runUntilCollided0 track iter (c:cs) cars =
      let c' = moveCart track c
      in  if (Set.null $ Set.filter (\c -> pos c == pos c') cars) &&
             ([] == filter (\c -> pos c == pos c') cs)
          then runUntilCollided0 track iter cs (Set.insert c' cars)
          else return (iter, pos c')

{-
There isn't much you can do to prevent crashes in this ridiculous system. However, by predicting the crashes, the
Elves know where to be in advance and instantly remove the two crashing carts the moment any crash occurs.

They can proceed like this for a while, but eventually, they're going to run out of carts. It could be useful to
figure out where the last cart that hasn't crashed will end up.

For example:

/>-<\
|   |
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/

/---\
|   |
| v-+-\
| | | |
\-+-/ |
  |   |
  ^---^

/---\
|   |
| /-+-\
| v | |
\-+-/ |
  ^   ^
  \---/

/---\
|   |
| /-+-\
| | | |
\-+-/ ^
  |   |
  \---/

After four very expensive crashes, a tick ends with only one cart remaining; its final location is 6,4.

What is the location of the last cart at the end of the first tick where it is the only cart left?
-}

sampleTrack =
  [ "/->-\\"
  , "|   |  /----\\"
  , "| /-+--+-\\  |"
  , "| | |  | v  |"
  , "\\-+-/  \\-+--/"
  , "  \\------/"
  ]

day13b :: [String] -> IO ()
day13b ls = do
  let (layout, cars) = parse ls
  result <- runUntilExploded layout 0 cars
  putStrLn $ show result


runUntilExploded track iter cars = do
  runUntilCollided0 track iter (Set.toAscList cars) Set.empty
  where
    runUntilCollided0 track iter [] cars = do
      if (length cars) == 1
      then return cars
      else do
        putStrLn ("Iteration " ++ (show iter) ++ " " ++ (show cars))
        runUntilCollided0 track (iter + 1) (Set.toAscList cars) Set.empty
    runUntilCollided0 track iter (c:cs) cars =
      let c' = moveCart track c
      in  if (not $ Set.null $ Set.filter (\c -> pos c == pos c') cars)
          then runUntilCollided0 track iter cs (Set.filter (\c -> pos c /= pos c') cars)
          else if ([] /= filter (\c -> pos c == pos c') cs)
          then runUntilCollided0 track iter (filter (\c -> pos c /= pos c') cs) cars
          else runUntilCollided0 track iter cs (Set.insert c' cars)