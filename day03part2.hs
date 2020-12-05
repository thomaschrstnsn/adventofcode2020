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

plotSlope :: [String] -> Slope -> String
plotSlope area slope = ps area slope (0, 0) []
  where
    ps :: [String] -> Slope -> Position -> String -> String
    ps [] _ _ rs = reverse rs
    ps (x : xs) (sx, sy) (px, py) rs = ps xs' (sx, sy) p' (char : rs)
      where
        row = cycle x
        char = row !! px
        p' = (px + sx, py + sy)
        xs' = drop (sy - 1) xs

countColisionsOnSlope :: [String] -> Slope -> Int
countColisionsOnSlope area slope = plotSlope area slope |> filter (== tree) |> length

allSlopes :: [(Int, Int)]
allSlopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

checkAllSlopes :: [String] -> [Int]
checkAllSlopes area = map (countColisionsOnSlope area) allSlopes

multiplyAllSlopes :: [String] -> Int
multiplyAllSlopes = product <$> checkAllSlopes

tests :: SpecWith ()
tests = do
  describe "countColisionsOnSlope" $
    it "works on example" $
      countColisionsOnSlope input (3, 1) `shouldBe` 7
  describe "checkAllSlopes" $
    it "works on example" $
      checkAllSlopes input `shouldBe` [2, 7, 3, 4, 2]
  describe "multiplyAllSlopes" $
    it "works on example" $
      multiplyAllSlopes input `shouldBe` 336
  where
    -- describe "comp" $
    --   it "comps" $
    --     let js = "....##..###.....#..............#...#..#.###.#.................#..##..#.##.#............##...##.#....###..##....#..#.#...#.....#......##..#...#............#......#"
    --         hs = "....##..###.....#..............#...#..#.###.#.................#..##..#.##.#............##...##.#....###..##....#..#.#...#.....#......##..#...#............#......."
    --      in hs `shouldBe` js

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
        solution = multiplyAllSlopes
      }
