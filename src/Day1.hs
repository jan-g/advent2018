module Day1 where

import Text.Read
import Data.Maybe
import Data.Set (Set, empty, insert, member)

day1 ls = do
  let ns = numLines ls
  let result = sum ns
  putStrLn (show result)

parse "" = Nothing
parse ('+' : s) = Just (read s)
parse ('-' : s) = Just (- read s)



{-
You notice that the device repeats the same frequency change list over and over. To calibrate the device, you need to find the first frequency it reaches twice.

For example, using the same list of changes above, the device would loop as follows:

    Current frequency  0, change of +1; resulting frequency  1.
    Current frequency  1, change of -2; resulting frequency -1.
    Current frequency -1, change of +3; resulting frequency  2.
    Current frequency  2, change of +1; resulting frequency  3.
    (At this point, the device continues from the start of the list.)
    Current frequency  3, change of +1; resulting frequency  4.
    Current frequency  4, change of -2; resulting frequency  2, which has already been seen.

In this example, the first frequency reached twice is 2. Note that your device might need to repeat its list of frequency changes many times before a duplicate frequency is found, and that duplicates might be found while in the middle of processing the list.

Here are other examples:

    +1, -1 first reaches 0 twice.
    +3, +3, +4, -2, -4 first reaches 10 twice.
    -6, +3, +8, +5, -6 first reaches 5 twice.
    +7, +7, -2, -7, -4 first reaches 14 twice.

What is the first frequency your device reaches twice?
-}

day1b ls = do
  putStrLn $ show $ findRepeat 0 empty $ partialSums $ cycle $ numLines $ lines "+3\n+3\n+4\n-2\n-4\n"
  putStrLn $ show $ findRepeat 0 empty $ partialSums $ cycle $ numLines $ lines "+1\n-1\n"

  let ns = numLines ls
  putStrLn ("number of parts" ++ (show $ length ns))
  let fs = cycle ns
      sums = partialSums fs
      (at, repetition) = findRepeat 0 empty sums
  putStrLn (show (at,repetition))

partialSums = scanl (+) 0

findRepeat count ns (s:ss) =
  if member s ns
  then (count, s)
  else findRepeat (count + 1) (insert s ns) ss

numLines :: [String] -> [Integer]
numLines contents = catMaybes $ map parse contents
