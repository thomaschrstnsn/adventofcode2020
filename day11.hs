#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
  --package hspec
  --package hspec-core
  --package flow
  --package formatting
  --package clock
  --package vector
-}

import Common ((|>))
import qualified Common
import Data.Maybe (catMaybes)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Specs (specFromExamples)
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )
import Test.Hspec.Core.Spec (specItem)

data Field = Occupied | Empty | Floor deriving (Eq)

instance Show Field where
  show Empty = "L"
  show Floor = "."
  show Occupied = "#"

readField :: Char -> Field
readField 'L' = Empty
readField '.' = Floor
readField '#' = Occupied
readField c = error $ "could not read value: '" ++ show c ++ "' as field (either . or L or # expected)"

newtype FieldSet = FieldSet {unFieldSet :: Vector (Vector Field)} deriving (Eq)

instance Show FieldSet where
  show fs = unlines $ map (concatMap show) $ toList fs

readFields :: [String] -> FieldSet
readFields lines = map readLine lines |> fromList
  where
    readLine l = map readField l

type Coordinate = (Int, Int)

getField :: FieldSet -> Coordinate -> Maybe Field
getField (FieldSet fs) (x, y) = do
  row <- fs !? y
  row !? x

fromList :: [[Field]] -> FieldSet
fromList rows = FieldSet $ Vector.fromList $ map Vector.fromList rows

toList :: FieldSet -> [[Field]]
toList (FieldSet fs) = Vector.map Vector.toList fs |> Vector.toList

clone :: FieldSet -> FieldSet
clone = fromList . toList

neighbours :: FieldSet -> Coordinate -> [Field]
neighbours _ (x, y) | x < 0 || y < 0 = error $ "invalid coordinate to lookup neighbours for: " ++ show (x, y)
neighbours fs (x, y) = catMaybes $ getField fs <$> candidates
  where
    candidates =
      [ (x, y -1),
        (x, y + 1),
        (x + 1, y),
        (x + 1, y + 1),
        (x + 1, y - 1),
        (x - 1, y),
        (x - 1, y - 1),
        (x - 1, y + 1)
      ]

updateField :: FieldSet -> Coordinate -> Field -> Field
updateField _ _ Floor = Floor
updateField fs c Empty = if Occupied `notElem` neighbours fs c then Occupied else Empty
updateField fs c Occupied = if occupiedNeighbours >= 4 then Empty else Occupied
  where
    occupiedNeighbours = length $ filter (== Occupied) (neighbours fs c)

runOnce :: FieldSet -> FieldSet
runOnce (FieldSet fs) = FieldSet $ Vector.imap (\y row -> Vector.imap (\x f -> updateField (FieldSet fs) (x, y) f) row) fs

countOccupied :: FieldSet -> Int
countOccupied fs = length $ filter (== Occupied) $ concat $ toList fs

solve' :: Int -> FieldSet -> (Int, Int)
solve' n fs = if done then (n, countOccupied fs) else solve' (n + 1) fs'
  where
    fs' = runOnce fs
    done = fs == fs'

solve :: FieldSet -> (Int, Int)
solve = solve' 0

tests :: SpecWith ()
tests = do
  describe "getField" $
    specFromExamples
      [((0, 0), Empty), ((1, 0), Floor), ((0, 6), Floor), ((1, 6), Floor), ((2, 6), Empty)]
      ( \(input, expected) ->
          specItem ("getField on " ++ show input ++ " should be: " ++ show expected) $
            getField parsed input `shouldBe` Just expected
      )

  describe "solve" $ it "with example" $ solve parsed `shouldBe` (5, 37)

  describe "runOnce" $ do
    it "with example" $
      let replacement 'L' = '#'
          replacement c = c
          textual = map (map replacement) input
          expected = readFields textual
       in runOnce parsed `shouldBe` expected
    it "twice" $
      run 2
        `shouldBe` readFields
          [ "#.LL.L#.##",
            "#LLLLLL.L#",
            "L.L.L..L..",
            "#LLL.LL.L#",
            "#.LL.LL.LL",
            "#.LLLL#.##",
            "..L.L.....",
            "#LLLLLLLL#",
            "#.LLLLLL.L",
            "#.#LLLL.##"
          ]
    it "thrice" $
      run 3
        `shouldBe` readFields
          [ "#.##.L#.##",
            "#L###LL.L#",
            "L.#.#..#..",
            "#L##.##.L#",
            "#.##.LL.LL",
            "#.###L#.##",
            "..#.#.....",
            "#L######L#",
            "#.LL###L.L",
            "#.#L###.##"
          ]
    it "four times" $
      run 4
        `shouldBe` readFields
          [ "#.#L.L#.##",
            "#LLL#LL.L#",
            "L.L.L..#..",
            "#LLL.##.L#",
            "#.LL.LL.LL",
            "#.LL#L#.##",
            "..L.L.....",
            "#L#LLLL#L#",
            "#.LLLLLL.L",
            "#.#L#L#.##"
          ]
    it "with example 5 times is stable" $ run 5 `shouldBe` stable
    it "with example 6 times is also stable" $ run 6 `shouldBe` stable
  where
    runs = iterate runOnce parsed
    run x = drop x runs |> head
    input =
      [ "L.LL.LL.LL",
        "LLLLLLL.LL",
        "L.L.L..L..",
        "LLLL.LL.LL",
        "L.LL.LL.LL",
        "L.LLLLL.LL",
        "..L.L.....",
        "LLLLLLLLLL",
        "L.LLLLLL.L",
        "L.LLLLL.LL"
      ]
    stable =
      [ "#.#L.L#.##",
        "#LLL#LL.L#",
        "L.#.L..#..",
        "#L##.##.L#",
        "#.#L.LL.LL",
        "#.#L#L#.##",
        "..L.L.....",
        "#L#L##L#L#",
        "#.LLLLLL.L",
        "#.#L#L#.##"
      ]
        |> readFields
    parsed = readFields input

main :: IO ()
main = do
  Common.aoc
    11
    Common.Solution
      { Common.parse = readFields . lines,
        Common.test = const tests,
        Common.solution = solve
      }
