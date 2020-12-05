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

tree :: Char
tree = '#'

type Slope = (Int, Int)

type Position = (Int, Int)

countColisionsOnCourse :: [String] -> Slope -> Int
countColisionsOnCourse area (3, 1) = ccoc area (3, 1) (0, 0) 0
  where
    ccoc :: [String] -> Slope -> Position -> Int -> Int
    ccoc [] _ _ n = n
    ccoc (x : xs) (sx, sy) (px, py) n = ccoc xs (sx, sy) p' n'
      where
        row = cycle x
        char = row !! px
        n' = if char == tree then n + 1 else n
        p' = (px + sx, py + sy)
countColisionsOnCourse _ _ = error "unsupported slope"

tests :: SpecWith ()
tests = do
  describe "countColisionsOnCourse" $
    it "works on example" $
      countColisionsOnCourse input (3, 1) `shouldBe` 7
  where
    input =
      [ "..##.......",
        "#...#...#..",
        ".#....#..#.",
        "..#.#...#.#",
        ".#...##..#.",
        "..#.##.....",
        ".#.#.#....#",
        ".#........#",
        "#.##...#...",
        "#...##....#",
        ".#..#...#.#"
      ]

main :: IO ()
main = do
  aoc
    3
    Solution
      { parse = lines,
        test = const tests,
        solution = \area -> countColisionsOnCourse area (3, 1)
      }
