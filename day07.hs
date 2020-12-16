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

import qualified Common
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
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

containedInMap :: [Rule] -> Map BagColor [BagColor]
containedInMap = foldl addToContainedMap Map.empty
  where
    addToContainedMap' :: BagColor -> Map BagColor [BagColor] -> BagColor -> Map BagColor [BagColor]
    addToContainedMap' outerBag m innerBag = Map.insertWith (++) innerBag [outerBag] m

    addToContainedMap :: Map BagColor [BagColor] -> Rule -> Map BagColor [BagColor]
    addToContainedMap m (_, NoFurtherBags) = m
    addToContainedMap m (outerBag, CountedBags innerBags) = foldl (addToContainedMap' outerBag) m $ map snd innerBags

countBagsThatCanContainBag :: BagColor -> [Rule] -> Int
countBagsThatCanContainBag color rules = Set.size $ bagsThatCanContainBag color rules

bagsThatCanContainBag :: BagColor -> [Rule] -> Set BagColor
bagsThatCanContainBag color rules = bagsThatContainBag' Set.empty (lookupInContained color)
  where
    cInMap = containedInMap rules

    lookupInContained :: BagColor -> [BagColor]
    lookupInContained c = fromMaybe [] $ Map.lookup c cInMap

    bagsThatContainBag' :: Set BagColor -> [BagColor] -> Set BagColor
    bagsThatContainBag' res [] = res
    bagsThatContainBag' res (x : xs) = bagsThatContainBag' res' xs'
      where
        xContainedIn = lookupInContained x
        res' = Set.unions [res, Set.fromList xContainedIn, Set.singleton x]
        xs' = xs ++ xContainedIn

tests :: SpecWith ()
tests = do
  describe "parse" $ do
    it "works on first example" $
      head <$> readRules input `shouldBe` Right ("light red", CountedBags [(1, "bright white"), (2, "muted yellow")])
  describe "bagsThatCanContainBag" $
    it "works on example" $
      bagsThatCanContainBag "shiny gold" <$> parsed `shouldBe` Right (Set.fromList ["bright white", "muted yellow", "dark orange", "light red"])
  describe "countBagsThatCanContainBag" $
    it "works on example" $
      countBagsThatCanContainBag "shiny gold" <$> parsed `shouldBe` Right 4
  where
    input =
      intercalate
        "\n"
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

main :: IO ()
main = do
  Common.aoc
    7
    Common.Solution
      { Common.parse = readRules,
        Common.test = const tests,
        Common.solution = fmap (countBagsThatCanContainBag "shiny gold")
      }
