{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Common (Solution (..), aoc, (|>), tbd) where

import Control.Exception (evaluate)
import Flow ((|>))
import Formatting (fprint, (%))
import Formatting.Clock (timeSpecs)
import System.Clock (Clock (Monotonic), getTime)
import Test.Hspec (SpecWith, hspec)
import Text.Printf (printf)

data Solution a b
  = Solution
      { parse :: String -> a,
        solution :: a -> b,
        test :: a -> SpecWith ()
      }
  | EitherSolution
      { parseEither :: String -> Either String a,
        solution :: a -> b,
        test :: a -> SpecWith ()
      }

benchmark :: IO a -> IO ()
benchmark action = do
  start <- getTime Monotonic
  action
  end <- getTime Monotonic
  fprint (" (" % timeSpecs % ")\n") start end

fileForDay :: Int -> String
fileForDay = printf "day%02dinput.txt"

bTests :: SpecWith () -> IO ()
bTests t =
  benchmark $ do
    hspec t
    putStr "Tests:"

bSol :: (Show b) => (a -> b) -> a -> IO ()
bSol sol prob = benchmark $ putStr $ "Solution: " ++ show (sol prob)

aoc :: (Show b) => Int -> Solution a b -> IO ()
aoc n Solution {solution, parse, test} = do
  input <- readFile $ fileForDay n
  let problem = parse input
  benchmark $ do
    putStr "Parsing input..."
    evaluate problem
  bTests (test problem)
  bSol solution problem
aoc n EitherSolution {solution, parseEither, test} = do
  input <- readFile $ fileForDay n
  let problemE = parseEither input
  benchmark $ do
    putStr "Parsing input..."
    evaluate problemE
  case problemE of
    Left err -> putStrLn $ "Error parsing: " ++ show (fileForDay n) ++ ": " ++ err
    Right prob -> do
      bTests $ test prob
      bSol solution prob

tbd :: a -> String
tbd _ = "(not implemented)"
