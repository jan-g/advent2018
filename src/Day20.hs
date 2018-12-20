module Day20
    ( day20
    , day20b
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


{-
While you were learning about instruction pointers, the Elves made considerable progress. When you look up, you
discover that the North Pole base construction project has completely surrounded you.

The area you are in is made up entirely of rooms and doors. The rooms are arranged in a grid, and rooms only connect
to adjacent rooms when a door is present between them.

For example, drawing rooms as ., walls as #, doors as | or -, your current position as X, and where north is up, the
area you're in might look like this:

#####
#.|.#
#-###
#.|X#
#####

You get the attention of a passing construction Elf and ask for a map. "I don't have time to draw out a map of this
place - it's huge. Instead, I can give you directions to every room in the facility!" He writes down some directions
on a piece of parchment and runs off. In the example above, the instructions might have been ^WNE$, a regular
expression or "regex" (your puzzle input).

The regex matches routes (like WNE for "west, north, east") that will take you from your current room through various
doors in the facility. In aggregate, the routes will take you through every door in the facility at least once; mapping
out all of these routes will let you build a proper map and find your way around.

^ and $ are at the beginning and end of your regex; these just mean that the regex doesn't match anything outside the
routes it describes. (Specifically, ^ matches the start of the route, and $ matches the end of it.) These characters
will not appear elsewhere in the regex.

The rest of the regex matches various sequences of the characters N (north), S (south), E (east), and W (west). In
the example above, ^WNE$ matches only one route, WNE, which means you can move west, then north, then east from your
current position. Sequences of letters like this always match that exact route in the same order.

Sometimes, the route can branch. A branch is given by a list of options separated by pipes (|) and wrapped in
parentheses. So, ^N(E|W)N$ contains a branch: after going north, you must choose to go either east or west before
finishing your route by going north again. By tracing out the possible routes after branching, you can determine where
the doors are and, therefore, where the rooms are in the facility.

For example, consider this regex: ^ENWWW(NEEE|SSE(EE|N))$

This regex begins with ENWWW, which means that from your current position, all routes must begin by moving east, north,
and then west three times, in that order. After this, there is a branch. Before you consider the branch, this is what
you know about the map so far, with doors you aren't sure about marked with a ?:

#?#?#?#?#
?.|.|.|.?
#?#?#?#-#
    ?X|.?
    #?#?#

After this point, there is (NEEE|SSE(EE|N)). This gives you exactly two options: NEEE and SSE(EE|N). By following NEEE,
the map now looks like this:

#?#?#?#?#
?.|.|.|.?
#-#?#?#?#
?.|.|.|.?
#?#?#?#-#
    ?X|.?
    #?#?#

Now, only SSE(EE|N) remains. Because it is in the same parenthesized group as NEEE, it starts from the same room NEEE
started in. It states that starting from that point, there exist doors which will allow you to move south twice, then
east; this ends up at another branch. After that, you can either move east twice or north once. This information fills
in the rest of the doors:

#?#?#?#?#
?.|.|.|.?
#-#?#?#?#
?.|.|.|.?
#-#?#?#-#
?.?.?X|.?
#-#-#?#?#
?.|.|.|.?
#?#?#?#?#

Once you've followed all possible routes, you know the remaining unknown parts are all walls, producing a finished map
of the facility:

#########
#.|.|.|.#
#-#######
#.|.|.|.#
#-#####-#
#.#.#X|.#
#-#-#####
#.|.|.|.#
#########

Sometimes, a list of options can have an empty option, like (NEWS|WNSE|). This means that routes at this point could
effectively skip the options in parentheses and move on immediately. For example, consider this regex and the
corresponding map:

^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$

###########
#.|.#.|.#.#
#-###-#-#-#
#.|.|.#.#.#
#-#####-#-#
#.#.#X|.#.#
#-#-#####-#
#.#.|.|.|.#
#-###-###-#
#.|.|.#.|.#
###########

This regex has one main route which, at three locations, can optionally include additional detours and be valid:
(NEWS|), (WNSE|), and (SWEN|). Regardless of which option is taken, the route continues from the position it is left
at after taking those steps. So, for example, this regex matches all of the following routes (and more that aren't
listed here):

    ENNWSWWSSSEENEENNN
    ENNWSWWNEWSSSSEENEENNN
    ENNWSWWNEWSSSSEENEESWENNNN
    ENNWSWWSSSEENWNSEEENNN

By following the various routes the regex matches, a full map of all of the doors and rooms in the facility can be
assembled.

To get a sense for the size of this facility, you'd like to determine which room is furthest from you: specifically,
you would like to find the room for which the shortest path to that room would require passing through the most doors.

    In the first example (^WNE$), this would be the north-east corner 3 doors away.
    In the second example (^ENWWW(NEEE|SSE(EE|N))$), this would be the south-east corner 10 doors away.
    In the third example (^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$), this would be the north-east corner 18 doors away.

Here are a few more examples:

Regex: ^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$
Furthest room requires passing 23 doors

#############
#.|.|.|.|.|.#
#-#####-###-#
#.#.|.#.#.#.#
#-#-###-#-#-#
#.#.#.|.#.|.#
#-#-#-#####-#
#.#.#.#X|.#.#
#-#-#-###-#-#
#.|.#.|.#.#.#
###-#-###-#-#
#.|.#.|.|.#.#
#############

Regex: ^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$
Furthest room requires passing 31 doors

###############
#.|.|.|.#.|.|.#
#-###-###-#-#-#
#.|.#.|.|.#.#.#
#-#########-#-#
#.#.|.|.|.|.#.#
#-#-#########-#
#.#.#.|X#.|.#.#
###-#-###-#-#-#
#.|.#.#.|.#.|.#
#-###-#####-###
#.|.#.|.|.#.#.#
#-#-#####-#-#-#
#.#.|.|.|.#.|.#
###############

What is the largest number of doors you would be required to pass through to reach a room? That is, find the room for
which the shortest path from your starting location to that room would require passing through the most doors; what is
the fewest doors you can pass through to reach it?


-}


parse line =
  let Just re = parseWith reParser line
  in  re

data RE = Move String
        | Cat [RE]
        | Alt [RE]
        | Anchor RE

reParser :: ReadP RE
reParser = do
  char '^'
  cat <- catParser
  char '$'
  eof
  return $ Anchor cat

catParser = do
  parts <- many partParser
  return $ Cat parts

partParser = altParser <++ moveParser

altParser = do
  char '('
  alts <- sepBy (catParser<++nothingParser) (char '|')
  char ')'
  return $ Alt alts

moveParser = do
  m <- munch1 (isAlpha)
  return $ Move m

nothingParser = return $ Move ""

instance Show RE where
  show (Anchor re) = "^" ++ (show re) ++ "$"
  show (Cat cs) = concatMap show cs
  show (Move m) = m
  show (Alt as) = "(" ++ (List.intercalate "|" $ map show as) ++ ")"



d0 = "^ENWWW(NEEE|SSE(EE|N))$"
d1 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
d2 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" {- 23 doors -}
d3 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" {- 31 doors -}




day20 :: [String] -> IO ()
day20 ls = do
  let re = parse $ ls !! 0
      layout = trace re
  putStrLn $ show re
  putStrLn $ show layout
  let search = bfs layout [Set.singleton (0,0)]
  putStrLn $ show $ (length search - 1)

type Loc = (Integer, Integer)
type Door = (Loc, Loc)
type Layout = Set.Set Door


trace re =
  let (layout, _) = trace1 re
  in  layout


dxdy 'N' = (0,-1)
dxdy 'E' = (1, 0)
dxdy 'S' = (0,1)
dxdy 'W' = (-1,0)

offsetLoc to@(dx, dy) loc@(x,y) = (x + dx, y + dy)
offsetLayout :: Loc -> Layout -> Layout
offsetLayout to layout = Set.map (\(l1, l2) -> (offsetLoc to l1, offsetLoc to l2)) layout


{- trace1 returns a map, centred on (0,0) and a set of finishing locations for a given path -}
trace1 :: RE -> (Layout, Set.Set Loc)
trace1 (Anchor re) = trace1 re

trace1 (Move cs) =
  let (doors, loc) = foldl (\(doors, loc) c ->
                             let loc' = offsetLoc (dxdy c) loc
                             in  (Set.insert (loc `min` loc', loc `max` loc') doors, loc'))
                           (Set.empty, (0,0)) cs
  in (doors, Set.singleton loc)

trace1 (Alt as) =
  {- each alternate is just traced out. The result is the union of maps, and the union of end locations -}
  foldl (\(doors, loc) (doors', loc') -> (Set.union doors doors', Set.union loc loc'))
        (Set.empty, Set.empty) $ map trace1 as

trace1 (Cat cs) =
  {- We begin at the end with an empty layout and {(0,0)} as all reachable locations.
     Working backwards, we take each RE piece and trace it. The result is a layout and
     set of intermediate locations.
     For each of those locations, we offset the layout we know is following and union it
     with the current set of doors. We offset all of the final locations by that dxdy also. -}
  foldr (\re (endDoors, endLocs) ->
          let (intermediateDoors, intermediateLocs) = trace1 re
              newDoors = foldMap (flip offsetLayout $ endDoors) intermediateLocs
              newLocs = foldMap ((flip Set.map) endLocs . offsetLoc) intermediateLocs
          in  (Set.union newDoors intermediateDoors, newLocs))
        (Set.empty, Set.singleton (0,0))
        cs

bfs :: Layout -> [Set.Set Loc] -> [Set.Set Loc]
bfs layout path =
  bfs0 layout path $ Set.unions path
  where
    bfs0 layout paths@(horizon:_) visited =
      let possibleLocations = foldMap (movesFrom layout) horizon
          {- allLocationsVisited = Set.unions paths -}
          newLocations = Set.difference possibleLocations visited
      in  if Set.null newLocations
          then paths
          else bfs0 layout (newLocations:paths) $ Set.union visited newLocations
      where
        movesFrom layout loc@(x,y) = Set.map fromJust $
                                     Set.filter isJust $
                                     Set.map (\(l1, l2) ->
                                       if loc == l1 then Just l2
                                       else if loc == l2 then Just l1
                                       else Nothing) layout
{-
Okay, so the facility is big.

How many rooms have a shortest path from your current location that pass through at least 1000 doors?
-}

infixl 2 ==>
a ==> b = b a

day20b :: [String] -> IO ()
day20b ls = do
  let re = parse $ ls !! 0
      layout = trace re
  putStrLn $ show re
  putStrLn $ show layout
  let search = bfs layout [Set.singleton (0,0)]
      result = reverse search ==> drop 1000 ==> map Set.size ==> sum
  putStrLn $ show $ (length search - 1)
  putStrLn $ show result
  putStrLn $ "The number of rooms is " ++ (show $ layout ==> rooms ==> Set.size)


rooms layout =
  Set.union
    (foldMap (\(l1,l2) -> Set.singleton l1) layout)
    (foldMap (\(l1,l2) -> Set.singleton l2) layout)


{- The following is a shitty first attempt that'll blow up.
   It starts at the start and works forward, which is clearly less than optimal.
   By contrast, trace1 is much smarter -}

trace0 :: Loc -> Layout -> RE -> Set.Set (Loc, Layout)
trace0 loc layout (Anchor re) = trace0 loc layout re

{-
trace0 loc layout (Cat []) = Set.singleton (loc, layout)
trace0 loc layout (Cat (c:cs)) =
  let (ll1s) = trace0 loc layout c
  in  foldMap (\(loc1, layout1) -> trace0 loc1 layout1 (Cat cs)) ll1s
-}

{- We do the cats backward, offsetting each part in turn -}
trace0 loc layout (Cat cs) =
  let lls = foldl (\lls re ->
                        {- trace this path piece from a new origin -}
                    let t0 = trace0 (0,0) Set.empty re  :: Set.Set (Loc, Layout)
                        {- for wherever this penultimate step might land,
                           offset the incoming map and endpoints -}
                    in  foldMap (\(loc,layout) -> Set.map (offset loc layout) lls) t0
                  ) (Set.singleton ((0,0), Set.empty)) $ reverse cs
  in Set.map (offset loc layout) lls


trace0 loc layout (Alt as) =
  foldMap (trace0 loc layout) as

trace0 loc layout (Move "") = Set.singleton (loc, layout)
trace0 loc@(x,y) layout (Move (m:ms)) =
  let (dx, dy) = dxdy m
      loc' = (x + dx, y + dy)
      layout' = Set.insert (loc `min` loc', loc `max` loc') layout
  in  trace0 loc' layout' (Move ms)

offset :: Loc -> Layout -> (Loc, Layout) -> (Loc, Layout)
offset to with (loc,layout) = (offsetLoc to loc, Set.union with $ offsetLayout to layout)
