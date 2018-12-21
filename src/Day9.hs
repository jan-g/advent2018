{-# LANGUAGE RecursiveDo #-}

module Day9
    ( day9
    , day9b
    , Game, runGame, maxScore, marbles
    , rotate
    , run2
    , run3
    ) where

import Lib
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Array ((!),(//))
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Array.MArray
import Control.Monad
import qualified Data.Sequence as Q

{-
You talk to the Elves while you wait for your navigation system to initialize. To pass the time, they introduce you to
their favorite marble game.

The Elves play this game by taking turns arranging the marbles in a circle according to very particular rules. The
marbles are numbered starting with 0 and increasing by 1 until every marble has a number.

First, the marble numbered 0 is placed in the circle. At this point, while it contains only a single marble, it is
still a circle: the marble is both clockwise from itself and counter-clockwise from itself. This marble is designated
the current marble.

Then, each Elf takes a turn placing the lowest-numbered remaining marble into the circle between the marbles that are
1 and 2 marbles clockwise of the current marble. (When the circle is large enough, this means that there is one marble
between the marble that was just placed and the current marble.) The marble that was just placed then becomes the
current marble.

However, if the marble that is about to be placed has a number which is a multiple of 23, something entirely different
happens. First, the current player keeps the marble they would have placed, adding it to their score. In addition,
the marble 7 marbles counter-clockwise from the current marble is removed from the circle and also added to the current
player's score. The marble located immediately clockwise of the marble that was removed becomes the new current marble.

For example, suppose there are 9 players. After the marble with value 0 is placed in the middle, each player (shown in
square brackets) takes a turn. The result of each of those turns would produce circles of marbles like this, where
clockwise is to the right and the resulting current marble is in parentheses:

[-] (0)
[1]  0 (1)
[2]  0 (2) 1
[3]  0  2  1 (3)
[4]  0 (4) 2  1  3
[5]  0  4  2 (5) 1  3
[6]  0  4  2  5  1 (6) 3
[7]  0  4  2  5  1  6  3 (7)
[8]  0 (8) 4  2  5  1  6  3  7
[9]  0  8  4 (9) 2  5  1  6  3  7
[1]  0  8  4  9  2(10) 5  1  6  3  7
[2]  0  8  4  9  2 10  5(11) 1  6  3  7
[3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
[4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
[5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
[6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
[7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
[8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
[9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
[1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
[2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
[3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
[4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
[5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
[6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
[7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

The goal is to be the player with the highest score after the last marble is used up. Assuming the example above ends
after the marble numbered 25, the winning score is 23+9=32 (because player 5 kept marble 23 and removed marble 9,
while no other player got any points in this very short example game).

Here are a few more examples:

    10 players; last marble is worth 1618 points: high score is 8317
    13 players; last marble is worth 7999 points: high score is 146373
    17 players; last marble is worth 1104 points: high score is 2764
    21 players; last marble is worth 6111 points: high score is 54718
    30 players; last marble is worth 5807 points: high score is 37305

What is the winning Elf's score?
-}

data Game = Game { marbles :: [Integer] {- Current position at the head -}
                 , scores :: Array.Array Integer Integer {- indexed from 1 by number of elves -}
                 }
  deriving (Show, Eq)

game n = Game { marbles=[0], scores=Array.array (1, n) [(i, 0) | i <- [1..n]]}

parseLine :: String -> Maybe (Integer, Integer)
parseLine s =
  case readP_to_S gameParser s of
    [] -> Nothing
    [(e, "")] -> Just e

gameParser :: ReadP (Integer, Integer)
gameParser = do
  numPlayers <- intParser
  string " players; last marble is worth "
  lastMarble <- intParser
  string " points"
  eof
  return $ (numPlayers, lastMarble)


day9 :: [String] -> IO ()
day9 ls = do
  let Just (np, lm) = parseLine $ ls !! 0
      gamen = runGame np lm
      result = maxScore gamen
  putStrLn $ (show np) ++ " players; last marble is worth " ++ (show lm) ++ " points: high score = " ++ (show result)


runGame np lm =
  let game0 = game np
  in foldl updateGame game0 $ [1 .. lm] `zip` (cycle [1 .. np])


maxScore g = maximum $ Array.elems $ scores g


rotate n as =
  let n' = n `mod` (length as)
  in  (drop n' as) ++ (take n' as)


updateGame :: Game -> (Integer, Integer) -> Game
updateGame Game {marbles=m0, scores=s0} (m, p) =
  if m `mod` 23 /= 0
  then
    Game { marbles=(m:(rotate 2 m0))
         , scores=s0
         }
  else
    let (m_7:ms) = rotate (-7) m0
        s1 = s0 // [(p, (s0 ! p) + m + m_7)]
    in
    Game { marbles=ms
         , scores=s1
         }

{-
Amused by the speed of your answer, the Elves are curious:

What would the new winning Elf's score be if the number of the last marble were 100 times larger?
-}

day9b :: [String] -> IO ()
day9b ls = do
  let Just (np, lm) = parseLine $ ls !! 0
      result = run2 np (lm * 100)
  putStrLn $ (show np) ++ " players; last marble is worth " ++ (show lm) ++ " points: high score = " ++ (show result)

data Node s = Node { val :: Integer
                   , p :: STRef s (Node s)
                   , n :: STRef s (Node s)
                   }


ring0 :: ST s (STRef s (Node s))
ring0 = do
  rec nR <- newSTRef Node { val=0, p=prev, n=next }
      node <- readSTRef nR
      prev <- newSTRef node
      next <- newSTRef node
  return nR

run2 np nm = runST $ do
  ring <- ring0
  sc <- newArray (0, np - 1) 0 :: ST s (STArray s Integer Integer)
  forM_ [1 .. nm] $ \v -> do
        if v `mod` 23 /= 0
        then do
          ring'n <- nextR ring
          ring'n'n <- readSTRef (n ring'n)
          ring' <- makeNode v ring'n ring'n'n
          writeSTRef ring ring'
        else do
          ring'p <- prevR ring
          ring'p'2 <- readSTRef (p ring'p)
          ring'p'3 <- readSTRef (p ring'p'2)
          ring'p'4 <- readSTRef (p ring'p'3)
          ring'p'5 <- readSTRef (p ring'p'4)
          ring'p'6 <- readSTRef (p ring'p'5)
          ring'p'7 <- readSTRef (p ring'p'6)
          writeSTRef ring ring'p'7
          score <- readArray sc (v `mod` np)
          v' <- valR ring
          writeArray sc (v `mod` np) (score + v + v')
          {- ring = ring.n -}
          writeSTRef ring ring'p'6
          {- ring.p = ring.p.p -}
          ring'p'8 <- readSTRef (p ring'p'7)
          writeSTRef (p ring'p'6) ring'p'8
          {- ring.p.n = ring -}
          writeSTRef (n ring'p'8) ring'p'6
  e <- getElems sc
  return $ maximum e

makeNode :: Integer -> Node s -> Node s -> ST s (Node s)
makeNode v prev next = do
  prevR <- newSTRef prev
  nextR <- newSTRef next
  let node = Node { val=v, p=prevR, n=nextR }
  writeSTRef (n prev) node
  writeSTRef (p next) node
  return node

nextR :: STRef s (Node s) -> ST s (Node s)
nextR nodeR = do
  node <- readSTRef nodeR
  node' <- readSTRef (n node)
  return node'

prevR :: STRef s (Node s) -> ST s (Node s)
prevR nodeR = do
  node <- readSTRef nodeR
  node' <- readSTRef (p node)
  return node'

valR :: STRef s (Node s) -> ST s (Integer)
valR nodeR = do
  node <- readSTRef nodeR
  return $ val node

{-


for m in range(1, top + 1):
    if m % 23 != 0:
        ring = Node(m, p=ring.n, n=ring.n.n)
    else:
        ring = ring.p.p.p.p.p.p.p
        players[m % np] += m + ring.val
        ring = ring.n
        ring.p = ring.p.p
        ring.p.n = ring

print(max(players))
-}

{- yet another version -}
newtype DLL s = DLL (Q.Seq s)
  deriving (Show)

{- For this type, we are focussed at the left end. One step to the last focusses on the current last:
   Focus : r1 : r2 : ... : l3 : l2 : l1
 -}
dllRing0 :: DLL Integer
dllRing0 = DLL (Q.singleton 0)

dllCurrent :: DLL s -> s
dllCurrent (DLL (a Q.:<| _)) = a

{- Drop and move one to the right -}
dllDrop :: DLL s -> (s, DLL s)
dllDrop (DLL (a Q.:<| rest)) = (a, DLL rest)

{- Move right -}
dllRight :: DLL s -> DLL s
dllRight (DLL (a Q.:<| rest)) = DLL $ rest Q.|> a

{- Move left -}
dllLeft (DLL (rest Q.:|> a)) = DLL $ a Q.<| rest

{- place one to the right of current, focussing on new -}
dllPlace :: s -> DLL s -> DLL s
dllPlace n (DLL (a Q.:<| rest)) = DLL $ (n Q.<| rest) Q.|> a

infixl 2 ==>
a ==> b = b a

run3 np nm =
  let (r, s) = foldl (\(r, s) m -> run3' r s m)
                     (dllRing0, Array.array (1, np) [(i, 0) | i <- [1..np]])
                     ([1 .. nm] `zip` (cycle [1 .. np]))

  in  Array.elems s ==> maximum
  where
    run3' :: DLL Integer -> Array.Array Integer Integer -> (Integer, Integer) -> (DLL Integer, Array.Array Integer Integer)
    run3' ring score (marb, pl)
      | marb `mod` 23 /= 0 = (ring ==> dllRight ==> dllPlace marb, score)
      | otherwise =
        let (v, ring' ) = ring ==> dllLeft ==> dllLeft ==> dllLeft ==> dllLeft ==>
                                   dllLeft ==> dllLeft ==> dllLeft ==> dllDrop
            score' = score // [(pl, (score ! pl) + marb + v)]
        in  (ring', score')