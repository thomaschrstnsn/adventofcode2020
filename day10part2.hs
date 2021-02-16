#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
  --package hspec
  --package hspec-core
  --package flow
  --package formatting
  --package clock
  --package containers
  --package vector
-}

import qualified Common
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )

readInt :: String -> Int
readInt = read

solve :: [Int] -> Int
solve = jolts

jolts :: [Int] -> Int
jolts xs = memoizedJolts (Set.fromList (end : xs)) end
  where
    end = maximum xs + 3

memoizedJolts :: Set Int -> Int -> Int
memoizedJolts input n = memo ! n
  where
    count 0 = 1
    count i | Set.member i input = one + two + three
      where
        w x = memo ! (i - x)
        one = w 1
        two = if i > 1 then w 2 else 0
        three = if i > 2 then w 3 else 0
    count _ = 0
    memo = Vector.generate (n + 1) count

tests :: SpecWith ()
tests = do
  describe "solve" $ do
    it "works with example" $
      solve parsed `shouldBe` 8
    it "works with another example" $
      let example =
            [ 28,
              33,
              18,
              42,
              31,
              14,
              46,
              20,
              48,
              47,
              24,
              23,
              49,
              45,
              19,
              38,
              39,
              11,
              1,
              32,
              25,
              35,
              8,
              17,
              7,
              9,
              4,
              2,
              34,
              10,
              3
            ]
       in solve example `shouldBe` 19208
  where
    input =
      [ "16",
        "10",
        "15",
        "5",
        "1",
        "11",
        "7",
        "19",
        "6",
        "12",
        "4"
      ]
    parsed = readInt <$> input

main :: IO ()
main = do
  Common.aoc
    10
    Common.Solution
      { Common.parse = \s -> readInt <$> lines s,
        Common.test = const tests,
        Common.solution = solve
      }
