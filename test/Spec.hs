import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Day7
import qualified Day8
import qualified Day9
import qualified Day11
import qualified Data.Set as Set
import Data.Array ((!))

main :: IO ()
main = hspec $ do
  describe "Day7" $ do
    it "correctly topSorts an empty graph" $ do
      topSort Set.findMin (empty :: Graph Int) `shouldBe` []

    it "correctly topSorts a minimal graph" $ do
      topSort Set.findMin
              ((empty :: Graph Int) `withEdge` (edge 1 2))
        `shouldBe` [1, 2]
      topSort Set.findMin
              ((empty :: Graph Int) `withEdge` (edge 2 1))
        `shouldBe` [2, 1]

    it "solves the example problem" $ do
      topSort Set.findMin
              ((empty :: Graph Char)
                `withEdge` (edge 'C' 'A')
                `withEdge` (edge 'C' 'F')
                `withEdge` (edge 'A' 'B')
                `withEdge` (edge 'A' 'D')
                `withEdge` (edge 'B' 'E')
                `withEdge` (edge 'D' 'E')
                `withEdge` (edge 'F' 'E')
              )
        `shouldBe` "CABDFE"

    it "solves the part two paroblem" $ do
      topSortWithWorkers Set.findMin timeTakenExample 2
              ((empty :: Graph Char)
                `withEdge` (edge 'C' 'A')
                `withEdge` (edge 'C' 'F')
                `withEdge` (edge 'A' 'B')
                `withEdge` (edge 'A' 'D')
                `withEdge` (edge 'B' 'E')
                `withEdge` (edge 'D' 'E')
                `withEdge` (edge 'F' 'E')
              )
        `shouldBe` [(3, 'C'),
                    (4, 'A'),
                    (6, 'B'),
                    (9, 'F'),
                    (10, 'D'),
                    (15, 'E')]

  describe "Day8" $ do
    it "parses a tree sequence" $ do
      Day8.parseLine "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
        `shouldBe` (Just $ Day8.tree [
                    Day8.tree [] [10, 11, 12],
                    Day8.tree [
                      Day8.tree [] [99]
                              ] [2]
                             ] [1, 1, 2])

    it "recursively sums metadata" $ do
      Day8.sumMeta (Day8.tree [
                 Day8.tree [] [10, 11, 12],
                 Day8.tree [
                           Day8.tree [] [99]
                           ] [2]
                 ] [1, 1, 2]) `shouldBe` 138

    it "evaluates node C" $ do
      Day8.evaluate (Day8.tree [Day8.tree [] [99]] [2]) `shouldBe` 0

    it "evaluates node B" $ do
      Day8.evaluate (Day8.tree [] [10, 11, 12]) `shouldBe` 33

    it "evaluates the root node" $ do
      Day8.evaluate (Day8.tree [Day8.tree [] [10, 11, 12], Day8.tree [Day8.tree [] [99]] [2]] [1, 1, 2])
        `shouldBe` 66

  describe "Day9" $ do
    it "rotates clockwise" $ do
      Day9.rotate 1 [1, 2, 3, 4] `shouldBe` [2, 3, 4, 1]

    it "rotates anticlockwise" $ do
      Day9.rotate (-1) [1, 2, 3, 4] `shouldBe` [4, 1, 2, 3]

    it "evaluates some basic games" $ do
      (Day9.maxScore $ Day9.runGame 9 25) `shouldBe` 32
    it "evaluates 1 move" $ do
      (Day9.marbles $ Day9.runGame 9 1) `shouldBe` [1, 0]
    it "evaluates 10 moves" $ do
      (Day9.marbles $ Day9.runGame 9 10) `shouldBe` [10, 5, 1, 6, 3, 7, 0, 8, 4, 9, 2]

    it "evaluates a longer game" $ do
      (Day9.maxScore $ Day9.runGame 10 1618) `shouldBe` 8317

    it "evaluates a much longer game" $ do
      (Day9.maxScore $ Day9.runGame 13 7999) `shouldBe` 146373

  describe "Day11" $ do
    it "computes values correctly" $ do
      Day11.cellValue 57 122 79 `shouldBe` -5
      Day11.cellValue 39 217 196 `shouldBe` 0
      Day11.cellValue 71 101 153 `shouldBe` 4

    it "computes the grid correctly" $ do
      let pls = Day11.makeArray 8
      pls ! (3, 5) `shouldBe` 4

    it "solves grid number 18" $ do
      let pls = Day11.makeArray 18
          ts = Day11.sumCells pls 3
          (x, y, m) = Day11.maxFor [1..298] [1..298] ts
      pls ! (33, 45) `shouldBe` 4
      pls ! (34, 46) `shouldBe` 3
      ts ! (33, 45) `shouldBe` 29
      (x, y, m) `shouldBe` (33, 45, 29)

    it "computes the right answer for n=16, grid #18" $ do
      let pls = Day11.makeArray 18
          (x, y, m) = Day11.calcMax pls 16
      (x, y, m) `shouldBe` (90, 269, 113)
