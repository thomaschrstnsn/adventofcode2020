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
  --package parsec
  --package vector
-}

{-# LANGUAGE NamedFieldPuns #-}

import qualified Common
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )
import Text.ParserCombinators.Parsec hiding (Parser, State)

data Instruction = Nop Int | Acc Int | Jmp Int
  deriving (Show, Eq)

readInstructions :: String -> Either String Program
readInstructions x = do
  let res = parse parser "in" x
  case res of
    (Left err) -> Left $ show err
    (Right r) -> Right $ Vector.fromList r

type Parser a = GenParser Char () a

parser :: Parser [Instruction]
parser = do
  result <- sepBy pInst newline
  _ <- many newline
  eof
  return result

pInst :: Parser Instruction
pInst = pInst' "nop" Nop <|> pInst' "acc" Acc <|> pInst' "jmp" Jmp

pInst' :: String -> (Int -> Instruction) -> Parser Instruction
pInst' inst ctor = do
  _ <- string inst
  _ <- space
  n <- pPrefixedInt
  return $ ctor n

pPrefixedInt :: Parser Int
pPrefixedInt = do
  sign <- char '+' <|> char '-'
  int <- pInt
  return $ if sign == '-' then (- int) else int

pInt :: Parser Int
pInt = readInt <$> many1 digit
  where
    readInt :: String -> Int
    readInt = read

data State = State {accumulator :: Int, ip :: Int, visited :: Set Int} deriving (Show, Eq)

type Program = Vector Instruction

findCompletingPermutation :: Program -> State
findCompletingPermutation program = head $ filter (completionDetector program) states
  where
    states = detectLoop <$> programPermutations [] 0 program

programPermutations :: [Program] -> Int -> Program -> [Program]
programPermutations res index program =
  if index == Vector.length program
    then res
    else programPermutations res' (index + 1) program
  where
    inst = program ! index
    newInst = case inst of
      Jmp x -> Nop x
      Nop x -> Jmp x
      x -> x
    permutation = if inst == newInst then [] else [Vector.update program (Vector.singleton (index, newInst))]
    res' = permutation ++ res

loopDetector :: State -> Bool
loopDetector State {ip, visited} = Set.member ip visited

completionDetector :: Program -> State -> Bool
completionDetector program State {ip} = ip >= Vector.length program

detectLoop :: Program -> State
detectLoop program = runUntil (\s -> completionDetector program s || loopDetector s) initial program

runUntil :: (State -> Bool) -> State -> Program -> State
runUntil pred state program =
  if pred state
    then state
    else runUntil pred (execute state program) program

initial :: State
initial = State {accumulator = 0, ip = 0, visited = Set.empty}

execute :: State -> Program -> State
execute State {ip, visited, accumulator} program = state'
  where
    instruction = program ! ip
    visited' = Set.insert ip visited

    ipDelta = case instruction of
      Jmp n -> n
      _ -> 1
    accDelta = case instruction of
      Acc n -> n
      _ -> 0

    ip' = ip + ipDelta
    accumulator' = accumulator + accDelta
    state' = State {ip = ip', accumulator = accumulator', visited = visited'}

tests :: SpecWith ()
tests = do
  describe "parse" $ do
    it "works on example" $
      Vector.toList <$> readInstructions input `shouldBe` Right [Nop 0, Acc 1, Jmp 4, Acc 3, Jmp (-3), Acc (-99), Acc 1, Jmp (-4), Acc 6]
  describe "findCompletingPermutation" $
    it "works on example" $
      findCompletingPermutation <$> parsed `shouldBe` Right State {accumulator = 8, ip = 9, visited = Set.fromList [0, 1, 2, 6, 7, 8]}
  where
    join = intercalate "\n"
    input =
      join
        [ "nop +0",
          "acc +1",
          "jmp +4",
          "acc +3",
          "jmp -3",
          "acc -99",
          "acc +1",
          "jmp -4",
          "acc +6"
        ]
    parsed = readInstructions input

main :: IO ()
main = do
  Common.aoc
    8
    Common.EitherSolution
      { Common.parseEither = readInstructions,
        Common.test = const tests,
        Common.solution = findCompletingPermutation
      }
