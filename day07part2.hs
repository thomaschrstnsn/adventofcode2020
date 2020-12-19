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
-}

import Common ((|>))
import qualified Common
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )
import Text.ParserCombinators.Parsec hiding (Parser)

type BagColor = String

data ContainedBag = NoFurtherBags | CountedBags [(Int, BagColor)]
  deriving (Show, Eq)

type Rule = (BagColor, ContainedBag)

readRules :: String -> Either String [Rule]
readRules x = do
  let res = parse parser "in" x
  case res of
    (Left err) -> Left $ show err
    (Right r) -> Right r

type Parser a = GenParser Char () a

parser :: Parser [Rule]
parser = do
  result <- sepBy pRule newline
  _ <- many newline
  eof
  return result

pRule :: Parser Rule
pRule = do
  container <- pContainerBag
  _ <- string " contain "
  contains <- pContainedBags
  _ <- char '.'
  return (container, contains)

pColor :: Parser String
pColor = do
  c1 <- word
  _ <- space
  c2 <- word
  return $ unwords [c1, c2]
  where
    word = many1 letter

pContainerBag :: Parser BagColor
pContainerBag = do
  color <- pColor
  _ <- space
  _ <- string "bags"
  return color

pInt :: Parser Int
pInt = readInt <$> many1 digit
  where
    readInt :: String -> Int
    readInt = read

pContainedBags :: Parser ContainedBag
pContainedBags = CountedBags <$> pNumberedBag `sepBy1` string ", " <|> pNoFurther
  where
    pNoFurther = NoFurtherBags <$ string "no other bags"

pNumberedBag :: Parser (Int, BagColor)
pNumberedBag = do
  n <- pInt
  _ <- space
  c <- pColor
  _ <- space
  _ <- if n == 1 then string "bag" else string "bags"
  return (n, c)

containsMap :: [Rule] -> Map BagColor [(Int, BagColor)]
containsMap rules = map toKeyValue rules |> Map.fromList
  where
    toKeyValue :: Rule -> (BagColor, [(Int, BagColor)])
    toKeyValue (k, NoFurtherBags) = (k, [])
    toKeyValue (k, CountedBags bags) = (k, bags)

numberOfBagsInsideBag :: BagColor -> [Rule] -> Int
numberOfBagsInsideBag color rules = sum $ bagsInsideBag color rules

bagsInsideBag :: BagColor -> [Rule] -> [Int]
bagsInsideBag color rules = bagsInsideBags' 1 (lookupInContains color)
  where
    cMap = containsMap rules

    lookupInContains :: BagColor -> [(Int, BagColor)]
    lookupInContains c = Map.findWithDefault (error "key not found") c cMap

    bagsInsideBags' :: Int -> [(Int, BagColor)] -> [Int]
    bagsInsideBags' mul xs = (* mul) <$> concatMap bagsInsideBag' xs

    bagsInsideBag' :: (Int, BagColor) -> [Int]
    bagsInsideBag' (n, c) = (* n) <$> bagsInsideBags' 1 (lookupInContains c) ++ [1]

tests :: SpecWith ()
tests = do
  describe "parse" $ do
    it "works on first example" $
      head <$> readRules input `shouldBe` Right ("light red", CountedBags [(1, "bright white"), (2, "muted yellow")])
  describe "numberOfBagsInsideBag" $ do
    it "works on example" $
      numberOfBagsInsideBag "shiny gold" <$> parsed
        `shouldBe` Right
          32
    it "works on second example" $
      numberOfBagsInsideBag "shiny gold" <$> parsed2 `shouldBe` Right 126
  where
    join = intercalate "\n"
    input =
      join
        [ "light red bags contain 1 bright white bag, 2 muted yellow bags.",
          "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
          "bright white bags contain 1 shiny gold bag.",
          "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
          "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
          "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
          "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
          "faded blue bags contain no other bags.",
          "dotted black bags contain no other bags."
        ]
    parsed = readRules input
    parsed2 =
      readRules $
        join
          [ "shiny gold bags contain 2 dark red bags.",
            "dark red bags contain 2 dark orange bags.",
            "dark orange bags contain 2 dark yellow bags.",
            "dark yellow bags contain 2 dark green bags.",
            "dark green bags contain 2 dark blue bags.",
            "dark blue bags contain 2 dark violet bags.",
            "dark violet bags contain no other bags."
          ]

main :: IO ()
main = do
  Common.aoc
    7
    Common.EitherSolution
      { Common.parseEither = readRules,
        Common.test = const tests,
        Common.solution = numberOfBagsInsideBag "shiny gold"
      }
