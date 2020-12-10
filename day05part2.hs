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

import Common ((|>))
import qualified Common
import Data.Bits (Bits (setBit))
import Data.List (sort)
import Specs (specFromExamples)
import Test.Hspec
  ( SpecWith,
    describe,
    shouldBe,
  )
import Test.Hspec.Core.Spec (specItem)

pbp :: (Char, Char) -> Int -> String -> Int -> Int
pbp _ _ [] r = r
pbp (down, up) bitNo (x : xs) res = pbp (down, up) (bitNo - 1) xs res'
  where
    res' = case (x == up, x == down) of
      (True, _) -> setBit res bitNo
      (False, True) -> res
      _ -> error $ "unexpected char in boarding pass: " ++ [x]

parseBoardingPass :: String -> (Int, Int)
parseBoardingPass s = (pbp ('F', 'B') (length row - 1) row 0, pbp ('L', 'R') (length col - 1) col 0)
  where
    (row, col) = splitAt 7 s

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

seatIds :: [String] -> [Int]
seatIds bps = seatId . parseBoardingPass <$> bps

solve :: [String] -> [Int]
solve bps = filter (not . (`elem` seats)) range
  where
    seats = seatIds bps |> sort
    min = head seats
    max = last seats
    range = [min .. max]

tests :: SpecWith ()
tests = do
  describe "parseBoardingPass" $
    specFromExamples
      [("FBFBBFFRLR", (44, 5)), ("BFFFBBFRRR", (70, 7)), ("FFFBBBFRRR", (14, 7)), ("BBFFBBFRLL", (102, 4))]
      ( \(input, expected) ->
          specItem (show input ++ " should be: " ++ show expected) $
            parseBoardingPass input `shouldBe` expected
      )

main :: IO ()
main = do
  Common.aoc
    5
    Common.Solution
      { Common.parse = lines,
        Common.test = const tests,
        Common.solution = solve
      }
