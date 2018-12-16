module Day12
    ( day12
    , day12b
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
The year 518 is significantly more underground than your history books implied. Either that, or you've arrived in a
vast cavern network under the North Pole.

After exploring a little, you discover a long tunnel that contains a row of small pots as far as you can see to your
left and right. A few of them contain plants - someone is trying to grow things in these geothermally-heated caves.

The pots are numbered, with 0 in front of you. To the left, the pots are numbered -1, -2, -3, and so on; to the right,
1, 2, 3.... Your puzzle input contains a list of pots from 0 to the right and whether they do (#) or do not (.)
currently contain a plant, the initial state. (No other pots currently contain plants.) For example, an initial state
of #..##.... indicates that pots 0, 3, and 4 currently contain plants.

Your puzzle input also contains some notes you find on a nearby table: someone has been trying to figure out how these
plants spread to nearby pots. Based on the notes, for each generation of plants, a given pot has or does not have a
plant based on whether that pot (and the two pots on either side of it) had a plant in the last generation. These are
written as LLCRR => N, where L are pots to the left, C is the current pot being considered, R are the pots to the
right, and N is whether the current pot will have a plant in the next generation. For example:

    A note like ..#.. => . means that a pot that contains a plant but with no plants within two pots of it will not
    have a plant in it during the next generation.
    A note like ##.## => . means that an empty pot with two plants on each side of it will remain empty in the next
    generation.
    A note like .##.# => # means that a pot has a plant in a given generation if, in the previous generation, there
    were plants in that pot, the one immediately to the left, and the one two pots to the right, but not in the ones
    immediately to the right and two to the left.

It's not clear what these plants are for, but you're sure it's important, so you'd like to make sure the current
configuration of plants is sustainable by determining what will happen after 20 generations.

For example, given the following input:

initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #

For brevity, in this example, only the combinations which do produce a plant are listed. (Your input includes all
possible combinations.) Then, the next 20 generations will look like this:

                 1         2         3
       0         0         0         0
 0: ...#..#.#..##......###...###...........
 1: ...#...#....#.....#..#..#..#...........
 2: ...##..##...##....#..#..#..##..........
 3: ..#.#...#..#.#....#..#..#...#..........
 4: ...#.#..#...#.#...#..#..##..##.........
 5: ....#...##...#.#..#..#...#...#.........
 6: ....##.#.#....#...#..##..##..##........
 7: ...#..###.#...##..#...#...#...#........
 8: ...#....##.#.#.#..##..##..##..##.......
 9: ...##..#..#####....#...#...#...#.......
10: ..#.#..#...#.##....##..##..##..##......
11: ...#...##...#.#...#.#...#...#...#......
12: ...##.#.#....#.#...#.#..##..##..##.....
13: ..#..###.#....#.#...#....#...#...#.....
14: ..#....##.#....#.#..##...##..##..##....
15: ..##..#..#.#....#....#..#.#...#...#....
16: .#.#..#...#.#...##...#...#.#..##..##...
17: ..#...##...#.#.#.#...##...#....#...#...
18: ..##.#.#....#####.#.#.#...##...##..##..
19: .#..###.#..#.#.#######.#.#.#..#.#...#..
20: .#....##....#####...#######....#.#..##.

The generation is shown along the left, where 0 is the initial state. The pot numbers are shown along the top, where
0 labels the center pot, negative-numbered pots extend to the left, and positive pots extend toward the right.
Remember, the initial state begins at pot 0, which is not the leftmost pot used in this example.

After one generation, only seven plants remain. The one in pot 0 matched the rule looking for ..#.., the one in pot 4
matched the rule looking for .#.#., pot 9 matched .##.., and so on.

In this example, after 20 generations, the pots shown as # contain plants, the furthest left of which is pot -2, and
the furthest right of which is pot 34. Adding up all the numbers of plant-containing pots after the 20th generation
produces 325.

After 20 generations, what is the sum of the numbers of all pots which contain a plant?
-}

data Gen = Gen { leftMost :: Integer
               , pots :: [Bool]
               }
  deriving (Eq)

instance Show Gen where
  show Gen { leftMost=lm, pots=ps } =
   (show lm) ++ " " ++ (map (\x -> if x then '#' else '.') ps)

data Rule = Rule { offset :: Integer
                 , items :: [Bool]
                 , result :: Bool
                 }
  deriving (Show, Eq)

parseHead :: String -> Maybe Gen
parseHead s =
  case readP_to_S headParser s of
    [] -> Nothing
    [(e, "")] -> Just e

headParser :: ReadP Gen
headParser = do
  string "initial state: "
  pots <- many1 potParser
  eof
  return Gen { leftMost=0, pots=pots }

potParser :: ReadP Bool
potParser = do
  ( do
    char '#'
    return True) <++ (
    do
    char '.'
    return False)

parseLine :: String -> Maybe Rule
parseLine s =
  case readP_to_S lineParser s of
    [] -> Nothing
    [(e, "")] -> Just e

lineParser :: ReadP Rule
lineParser = do
  pots <- many1 potParser
  string " => "
  result <- potParser
  eof
  return Rule { offset=(-2), items=pots, result=result }

day12 :: [String] -> IO ()
day12 ls = do
  let Just state = parseHead $ head ls
      rules = catMaybes $ map parseLine $ tail ls
      state' = simplify state
      rules' = Map.fromList [(items rule, result rule) | rule <- rules]
  putStrLn $ show state'
  let states = take 20 $ zip [1..] $ List.unfoldr (\state -> let s' = nextGen rules' state in Just (s', s')) state'

  forAllIO states (\(g, s) -> do
    putStrLn ((show g) ++ "  " ++ (show s) ++ "  " ++ (show $ evaluate s))
    )


simplify Gen { leftMost=lm, pots=[] } = Gen { leftMost=0, pots=[] }
simplify g@Gen { leftMost=lm, pots=(True:ps) } = Gen { leftMost=lm, pots=(True:dropWhileR not ps) }
simplify Gen { leftMost=lm, pots=(False:ps) } = simplify Gen {leftMost=lm + 1, pots=ps}

evaluate Gen { leftMost=lm, pots=[] } = 0
evaluate Gen { leftMost=lm, pots=(True:ps) } = lm + evaluate Gen { leftMost=lm+1, pots=ps }
evaluate Gen { leftMost=lm, pots=(False:ps) } = evaluate Gen { leftMost=lm+1, pots=ps }

nextGen rules Gen { leftMost=lm, pots=(p:ps) } =
  let newPots = nextGen0 False False False (False:p:ps)
  in  simplify $ Gen { leftMost=lm-2, pots = newPots }
  where
    nextGen0 l2 l1 c [] = [lookup l2 l1 c False False, lookup l1 c False False False, lookup c False False False False]
    nextGen0 l2 l1 c [r1] = [lookup l2 l1 c r1 False] ++ nextGen0 l1 c r1 []
    nextGen0 l2 l1 c (r1:r2:rs) = [lookup l2 l1 c r1 r2] ++ nextGen0 l1 c r1 (r2:rs)

    lookup l2 l1 c r1 r2 = rules Map.! [l2, l1, c, r1, r2]


{-
You realize that 20 generations aren't enough. After all, these plants will need to last another 1500 years to even
reach your timeline, not to mention your future.

After fifty billion (50000000000) generations, what is the sum of the numbers of all pots which contain a plant?
-}

day12b :: [String] -> IO ()
day12b ls = do
  let Just state = parseHead $ head ls
      rules = catMaybes $ map parseLine $ tail ls
      state' = simplify state
      rules' = Map.fromList [(items rule, result rule) | rule <- rules]
  putStrLn $ show state'
  let states = zip [1..] $ List.unfoldr (\state -> let s' = nextGen rules' state in Just (s', s')) state'
      (g, state, diff) = scanForRepeats states

      genDiff = 50000000000 - g
      finalState = state { leftMost=(leftMost state) + (diff * genDiff) }

  putStrLn ((show finalState) ++ "  " ++ (show $ evaluate finalState))

  where
    scanForRepeats (p0@(g,s0@Gen {leftMost=lm1, pots=pots1}):p1@(_,Gen {leftMost=lm2, pots=pots2}):ps)
      | pots1 == pots2 = (g, s0, lm2-lm1)
      | otherwise      = scanForRepeats (p1:ps)