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
import Data.Function
  ( (&),
  )
import Specs
  ( specFromExamples,
    specItem,
  )
import Test.Hspec
  ( SpecWith,
    describe,
    hspec,
    shouldBe,
  )

readInputs :: [String] -> [Int]
readInputs = map readInt
  where
    readInt :: String -> Int
    readInt = read

calcFuel :: Int -> Int
calcFuel modWeight = fuelWeights
  where
    fuelWeights = calcFuel' modWeight 0
    formula :: Int -> Int
    formula x = x `div` 3 - 2
    calcFuel' :: Int -> Int -> Int
    calcFuel' f acc = if continue then calcFuel' r (acc + r) else acc
      where
        r = formula f
        continue = r > 0

solve :: [Int] -> Int
solve xs = (calcFuel <$> xs) & sum

tests :: SpecWith ()
tests =
  describe "calcFuel" $
    specFromExamples
      [(14, 2), (1969, 966), (100756, 50346)]
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
