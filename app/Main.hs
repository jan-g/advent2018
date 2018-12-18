module Main where

import Lib
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17


import System.Environment


main = do
  args <- getArgs
  let action = case args !! 0 of
                 "day1" -> day1
                 "day1b" -> day1b
                 "day2" -> day2
                 "day2b" -> day2b
                 "day3" -> day3
                 "day3b" -> day3b
                 "day4" -> day4
                 "day4b" -> day4b
                 "day5" -> day5
                 "day5b" -> day5b
                 "day6" -> day6
                 "day6b" -> day6b
                 "day7" -> day7
                 "day7b" -> day7b
                 "day8" -> day8
                 "day8b" -> day8b
                 "day9" -> day9
                 "day9b" -> day9b
                 "day10" -> day10
                 "day10b" -> day10b
                 "day11" -> day11
                 "day11b" -> day11b
                 "day12" -> day12
                 "day12b" -> day12b
                 "day13" -> day13
                 "day13b" -> day13b
                 "day14" -> day14
                 "day14b" -> day14b
                 "day15" -> day15
                 "day15b" -> day15b
                 "day16" -> day16
                 "day16b" -> day16b
                 "day17" -> day17
                 "day17b" -> day17b
      source = args !! 1
  ls <- loadLines source
  action ls
