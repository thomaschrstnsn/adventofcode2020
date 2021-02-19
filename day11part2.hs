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

newtype Coordinate = Coordinate {unCoordinate :: (Int, Int)}

newtype Direction = Direction {unDirection :: (Int, Int)}

stepInDirection :: Coordinate -> Direction -> Coordinate
stepInDirection (Coordinate (x, y)) (Direction (dx, dy)) = Coordinate (x + dx, y + dy)

getField :: FieldSet -> Coordinate -> Maybe Field
getField (FieldSet fs) (Coordinate (x, y)) = do
  row <- fs !? y
  row !? x

fromList :: [[Field]] -> FieldSet
fromList rows = FieldSet $ Vector.fromList $ map Vector.fromList rows

toList :: FieldSet -> [[Field]]
toList (FieldSet fs) = Vector.map Vector.toList fs |> Vector.toList

clone :: FieldSet -> FieldSet
clone = fromList . toList

lookInDirection :: FieldSet -> Coordinate -> Direction -> Maybe Field
lookInDirection fs c dir =
  case field of
    (Just Floor) ->
      lookInDirection fs c' dir
    x -> x
  where
    c' = stepInDirection c dir
    field = getField fs c'

neighbours :: FieldSet -> Coordinate -> [Field]
neighbours _ (Coordinate (x, y)) | x < 0 || y < 0 = error $ "invalid coordinate to lookup neighbours for: " ++ show (x, y)
neighbours fs c = catMaybes $ lookInDirection fs c <$> directions
  where
    directions =
      Direction
        <$> [ (0, -1),
              (0, 1),
              (1, 0),
              (1, 1),
              (1, -1),
              (-1, 0),
              (-1, -1),
              (-1, 1)
            ]

updateField :: FieldSet -> Coordinate -> Field -> Field
updateField _ _ Floor = Floor
updateField fs c Empty = if Occupied `notElem` neighbours fs c then Occupied else Empty
updateField fs c Occupied = if occupiedNeighbours >= 5 then Empty else Occupied
  where
    occupiedNeighbours = length $ filter (== Occupied) (neighbours fs c)

runOnce :: FieldSet -> FieldSet
runOnce (FieldSet fs) = FieldSet $ Vector.imap (\y row -> Vector.imap (\x f -> updateField (FieldSet fs) (Coordinate (x, y)) f) row) fs

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
            getField parsed (Coordinate input) `shouldBe` Just expected
      )

  describe "solve" $ it "with example" $ solve parsed `shouldBe` (6, 26)
  where
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
