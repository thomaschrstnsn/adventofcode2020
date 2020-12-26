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
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )

readInt :: String -> Int
readInt = read

isValid :: [Int] -> Int -> Bool
isValid [] _ = False
isValid (p : preamble) n = ((n - p) `elem` preamble) || isValid preamble n

invalidNumbers :: Int -> [Int] -> [Int]
invalidNumbers preambleLength xs = invalidNumbers' [] (take preambleLength xs) (drop preambleLength xs)
  where
    invalidNumbers' :: [Int] -> [Int] -> [Int] -> [Int]
    invalidNumbers' res _ [] = reverse res
    invalidNumbers' res preamble (x : xs) = invalidNumbers' res' preamble' xs
      where
        preamble' = drop 1 preamble ++ [x]
        res' = if isValid preamble x then res else x : res

firstInvalidNumber :: Int -> [Int] -> Int
firstInvalidNumber preambleLength numbers = head $ invalidNumbers preambleLength numbers

contiguousSetSummingTo :: [Int] -> Int -> [Int]
contiguousSetSummingTo [] _ = []
contiguousSetSummingTo (x : xs) n = if null winner then contiguousSetSummingTo xs n else winner
  where
    winner = csst [x] xs x
    csst :: [Int] -> [Int] -> Int -> [Int]
    csst _ [] _ = []
    csst res (y : ys) sum = case compare (y + sum) n of
      LT -> csst (y : res) ys (y + sum)
      EQ -> reverse (y : res)
      GT -> []

solve :: Int -> [Int] -> Int
solve preambleLength xs = minimum cs + maximum cs
  where
    number = firstInvalidNumber preambleLength xs
    cs = contiguousSetSummingTo xs number

tests :: SpecWith ()
tests = do
  describe "readInt" $
    it "works with really big numbers" $
      readInt <$> ["100903791250742", "59992996162974"] `shouldBe` [100903791250742, 59992996162974]
  describe "firstInvalidNumber" $
    it "works with example" $
      firstInvalidNumber 5 parsed `shouldBe` 127
  describe "contiguousSetSummingTo" $
    it "works with example" $
      contiguousSetSummingTo parsed 127 `shouldBe` [15, 25, 47, 40]
  describe "solve" $
    it "works with example" $
      solve 5 parsed `shouldBe` 62
  where
    input =
      [ "35",
        "20",
        "15",
        "25",
        "47",
        "40",
        "62",
        "55",
        "65",
        "95",
        "102",
        "117",
        "150",
        "182",
        "127",
        "219",
        "299",
        "277",
        "309",
        "576"
      ]
    parsed = readInt <$> input

main :: IO ()
main = do
  Common.aoc
    9
    Common.Solution
      { Common.parse = \s -> readInt <$> lines s,
        Common.test = const tests,
        Common.solution = solve 25
      }
