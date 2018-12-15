module Day10
    ( day10
    , day10b
    , Star
    ) where

import Lib
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set


{-
It's no use; your navigation system simply isn't capable of providing walking directions in the arctic circle, and
certainly not in 1018.

The Elves suggest an alternative. In times like these, North Pole rescue operations will arrange points of light in the
sky to guide missing Elves back to base. Unfortunately, the message is easy to miss: the points move slowly enough that
it takes hours to align them, but have so much momentum that they only stay aligned for a second. If you blink at the
wrong time, it might be hours before another message appears.

You can see these points of light floating in the distance, and record their position in the sky and their velocity,
the relative change in position per second (your puzzle input). The coordinates are all given from your perspective;
given enough time, those positions and velocities will move the points into a cohesive message!

Rather than wait, you decide to fast-forward the process and calculate what the points will eventually spell.

For example, suppose you note the following points:

position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>



Each line represents one point. Positions are given as <X, Y> pairs: X represents how far left (negative) or right
(positive) the point appears, while Y represents how far up (negative) or down (positive) the point appears.

At 0 seconds, each point has the position given. Each second, each point's velocity is added to its position. So, a
point with velocity <1, -2> is moving to the right, but is moving upward twice as quickly. If this point's initial
position were <3, 9>, after 3 seconds, its position would become <6, 3>.

Over time, the points listed above would move like this:

Initially:
........#.............
................#.....
.........#.#..#.......
......................
#..........#.#.......#
...............#......
....#.................
..#.#....#............
.......#..............
......#...............
...#...#.#...#........
....#..#..#.........#.
.......#..............
...........#..#.......
#...........#.........
...#.......#..........

After 1 second:
......................
......................
..........#....#......
........#.....#.......
..#.........#......#..
......................
......#...............
....##.........#......
......#.#.............
.....##.##..#.........
........#.#...........
........#...#.....#...
..#...........#.......
....#.....#.#.........
......................
......................

After 2 seconds:
......................
......................
......................
..............#.......
....#..#...####..#....
......................
........#....#........
......#.#.............
.......#...#..........
.......#..#..#.#......
....#....#.#..........
.....#...#...##.#.....
........#.............
......................
......................
......................

After 3 seconds:
......................
......................
......................
......................
......#...#..###......
......#...#...#.......
......#...#...#.......
......#####...#.......
......#...#...#.......
......#...#...#.......
......#...#...#.......
......#...#..###......
......................
......................
......................
......................

After 4 seconds:
......................
......................
......................
............#.........
........##...#.#......
......#.....#..#......
.....#..##.##.#.......
.......##.#....#......
...........#....#.....
..............#.......
....#......#...#......
.....#.....##.........
...............#......
...............#......
......................
......................

After 3 seconds, the message appeared briefly: HI. Of course, your message will be much longer and will take many more
seconds to appear.

What message will eventually appear in the sky?


-}

data Star = Star { x :: Integer {- Starting position at the head -}
                 , y :: Integer
                 , dx :: Integer
                 , dy :: Integer
                 }
  deriving (Show, Eq)

star x y dx dy = Star { x=x, y=y, dx=dx, dy=dy }

parseLine :: String -> Maybe Star
parseLine s =
  case readP_to_S starParser s of
    [] -> Nothing
    [(e, "")] -> Just e

starParser :: ReadP Star
starParser = do
  string "position=<"
  skipSpaces
  x <- intParser
  char ','
  skipSpaces
  y <- intParser
  string "> velocity=<"
  skipSpaces
  dx <- intParser
  char ','
  skipSpaces
  dy <- intParser
  string ">"
  eof
  return $ star x y dx dy


day10 :: [String] -> IO ()
day10 ls = do
  let stars = catMaybes $ map parseLine ls
  putStrLn $ show stars
  let (minH, t) = firstMinimum Nothing Day10.span y_after_t stars [0 ..]
      (minW, t') = firstMinimum Nothing Day10.span x_after_t stars [0 ..]

  putStrLn $ ("At time " ++ (show t) ++ " the minimum height is " ++ (show minH))
  putStrLn $ ("At time " ++ (show t') ++ " the minimum width is " ++ (show minW))

  let minX = minimum $ map (x_after_t t) stars
      maxX = maximum $ map (x_after_t t) stars
      minY = minimum $ map (y_after_t t) stars
      maxY = maximum $ map (y_after_t t) stars
      coords = Set.fromList $ map (after_t t) stars

  printLine coords [minX .. maxX] [minY .. maxY]

  where
    printLine :: (Set.Set (Integer, Integer)) -> [Integer] -> [Integer] -> IO ()
    printLine _ _ [] = return ()
    printLine coords xs (y:ys) = do
      printCol coords y xs
      printLine coords xs ys
    printCol :: (Set.Set (Integer, Integer)) -> Integer -> [Integer] -> IO ()
    printCol coords _ [] = putChar '\n'
    printCol coords y (x:xs) = do
      putChar (if (x, y) `Set.member` coords then '*' else '.')
      printCol coords y xs

firstMinimum least f selector stars ts =
  let (t:ts') = ts
      value = f (selector t) stars
  in  case least of
        Nothing -> firstMinimum (Just (value, t)) f selector stars ts'
        Just (v', t') | value > v' -> (v', t')
        Just (v', t') | otherwise -> firstMinimum (Just (value, t)) f selector stars ts'

span selector stars =
  (maximum $ map selector stars) - (minimum $ map selector stars)

after_t t star = (x_after_t t star, y_after_t t star)

x_after_t t star =
  (x star) + t * (dx star)

y_after_t t star =
  (y star) + t * (dy star)


{-
-}

day10b :: [String] -> IO ()
day10b ls = do
  let stars = catMaybes $ map parseLine ls
  putStrLn $ (show stars)
