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
import Specs
  ( specFromExamples,
    specItem,
  )
import Test.Hspec
  ( SpecWith,
    describe,
    shouldBe,
  )

readInputs :: [String] -> [Int]
readInputs = map readInt
  where
    readInt :: String -> Int
    readInt = read

calcFuel :: Int -> Int
calcFuel x = x `div` 3 - 2

solve :: [Int] -> Int
solve xs = sum $ calcFuel <$> xs

tests :: SpecWith ()
tests =
  describe "calcFuel" $
    specFromExamples
      [(12, 2), (14, 2), (1969, 654), (100756, 33583)]
      ( \(input, expected) ->
          specItem (show input ++ " should be: " ++ show expected) $
            calcFuel input
              `shouldBe` expected
      )

main :: IO ()
main = do
  aoc
    1
    Solution
      { parse = readInputs . lines,
        test = const tests,
        solution = solve
      }
