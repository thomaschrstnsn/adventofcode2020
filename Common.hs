{-# LANGUAGE OverloadedStrings #-}

module Common (Solution (..), aoc, (|>), tbd) where

import Control.Exception (evaluate)
import Flow ((|>))
import Formatting (fprint, (%))
import Formatting.Clock (timeSpecs)
import System.Clock (Clock (Monotonic), getTime)
import Test.Hspec (SpecWith, hspec)
import Text.Printf (printf)

data Solution a b = Solution
  { parse :: String -> a,
    solution :: a -> b,
    test :: a -> SpecWith ()
  }

benchmark :: IO a -> IO ()
benchmark action = do
  start <- getTime Monotonic
  action
  end <- getTime Monotonic
  fprint (" (" % timeSpecs % ")\n") start end

aoc :: (Show b) => Int -> Solution a b -> IO ()
aoc n solution = do
  input <- readFile $ printf "day%02dinput.txt" n
  let problem = parse solution input
  benchmark $ do
    putStr "Parsing input..."
    evaluate problem
  benchmark $ do
    hspec (test solution problem)
    putStr "Tests:"
  benchmark $ putStr $ "Solution: " ++ show (Common.solution solution problem)

tbd :: a -> String
tbd _ = "(not implemented)"
