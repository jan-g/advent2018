module Day2 where

import Text.Read
import Data.Counter (Counter, empty, update)
import qualified Data.Map.Strict (filter, size)
import Data.Maybe

{-
To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number that have an ID
containing exactly two of any letter and then separately counting those with exactly three of any letter. You can
multiply those two counts together to get a rudimentary checksum and compare it to what your device predicts.

For example, if you see the following box IDs:

    abcdef contains no letters that appear exactly two or three times.
    bababc contains two a and three b, so it counts for both.
    abbcde contains two b, but no letter appears exactly three times.
    abcccd contains three c, but no letter appears exactly two times.
    aabcdd contains two a and two d, but it only counts once.
    abcdee contains two e.
    ababab contains three a and three b, but it only counts once.

Of these box IDs, four of them contain a letter which appears exactly twice, and three of them contain a letter which
appears exactly three times. Multiplying these together produces a checksum of 4 * 3 = 12.

What is the checksum for your list of box IDs?
-}

day2 ls = do
  let ch2 = checkN 2 ls
      ch3 = checkN 3 ls
      result = ch2 * ch3
  putStrLn (show result)

checkN :: Integer -> [String] -> Int
checkN n lines =
  length $ filter (hasN n) lines

hasN :: Integer -> String -> Bool
hasN n line =
  let f = lineFreq line
      m = Data.Map.Strict.filter (n ==) f
  in (Data.Map.Strict.size m) > 0

lineFreq :: String -> Counter Char Integer
lineFreq line =
  let counter = Data.Counter.empty :: Counter Char Integer
  in  foldl (flip update) counter line


{-
Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.

The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given
the following box IDs:

abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz

The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij
and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.

What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing
character from either ID, producing fgij.)
-}

day2b ls = do
  let result = pairwise (selector) ls
  putStrLn (show result)

pairwise :: (a -> a -> Maybe b) -> [a] -> [b]
pairwise f [] = []
pairwise f (x:xs) = (catMaybes $ map (f x) xs) ++ (pairwise f xs)

selector s1 s2 =
  let sim = similarities s1 s2
  in
  if (length sim) == (length s1) - 1
  then Just sim
  else Nothing

similarities (x:xs) (y:ys) =
  if x == y
  then x : (similarities xs ys)
  else similarities xs ys
similarities _ _ = ""