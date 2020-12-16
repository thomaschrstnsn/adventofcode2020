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
parse xs = p Set.empty [] xs
  where
    addToAnswers :: Answers -> String -> Answers
    addToAnswers a line = foldl (\s x -> Set.insert x s) a line

    p :: Answers -> [Answers] -> [String] -> [Answers]
    p cur res [] = reverse (cur : res)
    p cur res ("" : xs) = p Set.empty (cur : res) xs
    p cur res (x : xs) = p cur' res xs
      where
        cur' = addToAnswers cur x

sumCountOfAnswers :: [Answers] -> Int
sumCountOfAnswers as = sum $ map Set.size as

tests :: SpecWith ()
tests = do
  describe "sumCountOfAnswers" $
    it "works on example" $
      sumCountOfAnswers parsed `shouldBe` 11
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
