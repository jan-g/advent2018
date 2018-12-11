import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Day7
import qualified Day8
import qualified Data.Set as Set

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
