#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
  --package hspec
  --package hspec-core
  --package flow
  --package formatting
  --package clock
-}

import Common
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )

readInputs :: [String] -> [Int]
readInputs = map readInt
  where
    readInt :: String -> Int
    readInt = read

solve :: [Int] -> Maybe Int
solve [] = Nothing
solve (x : xs) = if candidate `elem` xs then Just $ x * candidate else solve xs
  where
    candidate = 2020 - x

tests :: SpecWith ()
tests =
  describe "solve" $
    it "works on example" $
      let input = [1721, 979, 366, 299, 675, 1456]
       in solve input `shouldBe` Just 514579

main :: IO ()
main = do
  aoc
    1
    Solution
      { parse = readInputs . lines,
        test = const tests,
        solution = solve
      }
