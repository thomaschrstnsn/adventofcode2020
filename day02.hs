#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
  --package hspec
  --package hspec-core
  --package flow
  --package formatting
  --package clock
  --package split
-}

{-# LANGUAGE NamedFieldPuns #-}

import Common
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )

data Policy = Policy {pMin :: Int, pMax :: Int, pChar :: Char} deriving (Show, Eq)

type Password = String

readInputs :: [String] -> [(Policy, Password)]
readInputs ls = parseLine <$> ls

parseLine :: String -> (Policy, Password)
parseLine l = (Policy {pMin = min, pMax = max, pChar = char}, dropWhile isSpace password)
  where
    readInt :: String -> Int
    readInt = read
    [policyString, password] = splitOn ":" l
    [minS, maxAndChar] = splitOn "-" policyString
    [maxS, charS] = splitOn " " maxAndChar
    [min, max] = readInt <$> [minS, maxS]
    char = head charS

isValidPassword :: Policy -> Password -> Bool
isValidPassword Policy {pMin, pMax, pChar} password = countOfChar >= pMin && countOfChar <= pMax
  where
    countOfChar = length $ filter (== pChar) password

countValidPasswords :: [(Policy, Password)] -> Int
countValidPasswords pws = filter (uncurry isValidPassword) pws |> length

tests :: SpecWith ()
tests = do
  describe "parse" $
    it "works as expected" $
      parseLine (head input) `shouldBe` (Policy {pMin = 1, pMax = 3, pChar = 'a'}, "abcde")
  describe "solve" $
    it "works on example" $
      (countValidPasswords . readInputs) input `shouldBe` 2
  where
    input =
      [ "1-3 a: abcde",
        "1-3 b: cdefg",
        "2-9 c: ccccccccc"
      ]

main :: IO ()
main = do
  aoc
    2
    Solution
      { parse = readInputs . lines,
        test = const tests,
        solution = countValidPasswords
      }
