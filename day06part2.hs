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
  --package split
-}

import Common ((|>))
import qualified Common
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )

type Answers = Set Char

parse :: [String] -> [Answers]
parse xs = p [] [] xs
  where
    setOfLine :: String -> Answers
    setOfLine line = Set.fromList line

    intersections :: [Answers] -> Answers
    intersections [] = Set.empty
    intersections (a : as) = foldl Set.intersection a as

    p :: [Answers] -> [Answers] -> [String] -> [Answers]
    p cur res [] = reverse (intersections cur : res)
    p cur res ("" : xs) = p [] (intersections cur : res) xs
    p cur res (x : xs) = p (group : cur) res xs
      where
        group = setOfLine x

sumCountOfAnswers :: [Answers] -> Int
sumCountOfAnswers as = map Set.size as |> sum

tests :: SpecWith ()
tests = do
  describe "sumCountOfAnswers" $
    it "works on example" $
      sumCountOfAnswers parsed `shouldBe` 6
  describe "parse" $
    it "works with example" $
      parsed `shouldBe` [Set.fromList "abc", Set.empty, Set.fromList "a", Set.fromList "a", Set.fromList "b"]
  where
    input =
      [ "abc",
        "",
        "a",
        "b",
        "c",
        "",
        "ab",
        "ac",
        "",
        "a",
        "a",
        "a",
        "a",
        "",
        "b"
      ]
    parsed = parse input

main :: IO ()
main = do
  Common.aoc
    6
    Common.Solution
      { Common.parse = parse . lines,
        Common.test = const tests,
        Common.solution = sumCountOfAnswers
      }
