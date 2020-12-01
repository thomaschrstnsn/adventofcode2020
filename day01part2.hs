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

solve2 :: Int -> [Int] -> Maybe Int
solve2 _ [] = Nothing
solve2 n (x : xs) = if candidate `elem` xs then Just $ x * candidate else solve2 n xs
  where
    candidate = n - x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

solve :: [Int] -> Maybe Int
solve [] = Nothing
solve (x : xs) =
  case winner of
    Just n -> Just $ x * n
    Nothing -> solve xs
  where
    limit = 2020 - x
    candidates = filter (\n -> n < limit) xs
    winner = solve2 limit candidates

tests :: SpecWith ()
tests =
  describe "solve" $
    it "works on example" $
      let input = [1721, 979, 366, 299, 675, 1456]
       in solve input `shouldBe` Just 241861950

main :: IO ()
main = do
  aoc
    1
    Solution
      { parse = readInputs . lines,
        test = const tests,
        solution = solve
      }
