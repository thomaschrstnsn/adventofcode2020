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
import Data.Either (rights)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )

type PotentialPassport = Map String String

parse :: [String] -> [PotentialPassport]
parse xs = p Map.empty [] xs
  where
    addToPassport :: PotentialPassport -> String -> PotentialPassport
    addToPassport p line = foldl (\m (k, v) -> Map.insert k v m) p kvs
      where
        pairs = splitOn " " line
        kv x = (k, v)
          where
            [k, v] = splitOn ":" x
        kvs = map kv pairs

    p :: PotentialPassport -> [PotentialPassport] -> [String] -> [PotentialPassport]
    p cur res [] = reverse (cur : res)
    p cur res ("" : xs) = p Map.empty (cur : res) xs
    p cur res (x : xs) = p cur' res xs
      where
        cur' = addToPassport cur x

requiredFields =
  Set.fromList
    [ "byr",
      "iyr",
      "eyr",
      "hgt",
      "hcl",
      "ecl",
      "pid"
    ]

validatePassport :: PotentialPassport -> Either (Set String) ()
validatePassport p = if missing == Set.empty then Right () else Left missing
  where
    keys = Set.union (Map.keysSet p) (Set.singleton "cid")
    missing = Set.difference requiredFields keys

countValidPassports :: [PotentialPassport] -> Int
countValidPassports ps = map validatePassport ps |> rights |> length

tests :: SpecWith ()
tests = do
  describe "countValidPassports" $
    it "works on example" $
      countValidPassports parsed `shouldBe` 2
  describe "validatePassport" $
    it "works on example" $
      map validatePassport parsed `shouldBe` [Right (), Left (Set.singleton "hgt"), Right (), Left (Set.singleton "byr")]
  where
    input =
      [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
        "byr:1937 iyr:2017 cid:147 hgt:183cm",
        "",
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
        "hcl:#cfa07d byr:1929",
        "",
        "hcl:#ae17e1 iyr:2013",
        "eyr:2024",
        "ecl:brn pid:760753108 byr:1931",
        "hgt:179cm",
        "",
        "hcl:#cfa07d eyr:2025 pid:166559648",
        "iyr:2011 ecl:brn hgt:59in"
      ]
    parsed = parse input

main :: IO ()
main = do
  Common.aoc
    4
    Common.Solution
      { Common.parse = parse . lines,
        Common.test = const tests,
        Common.solution = countValidPassports
      }
