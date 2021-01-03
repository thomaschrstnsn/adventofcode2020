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
-}

import qualified Common
import Data.List (sort)
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )

readInt :: String -> Int
readInt = read

solve :: [Int] -> Maybe Int
solve xs = do
  (ones, threes) <- jolts xs
  return $ ones * threes

jolts :: [Int] -> Maybe (Int, Int)
jolts xs = sortedJolts (0, 0) 0 (sort xs)

sortedJolts :: (Int, Int) -> Int -> [Int] -> Maybe (Int, Int)
sortedJolts (ones, threes) _ [] = Just (ones, threes + 1)
sortedJolts (ones, threes) prev (x : xs) =
  case res' of
    Just res'' -> sortedJolts res'' x xs
    Nothing -> Nothing
  where
    delta = x - prev
    res' = case delta of
      1 -> Just (ones + 1, threes)
      3 -> Just (ones, threes + 1)
      _ -> Nothing

tests :: SpecWith ()
tests = do
  describe "jolts" $ do
    it "works with example" $
      jolts parsed `shouldBe` Just (7, 5)
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
       in jolts example `shouldBe` Just (22, 10)
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
