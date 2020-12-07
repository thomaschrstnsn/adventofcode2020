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
  --package split
-}

import Common ((|>))
import qualified Common
import qualified Data.Char as Char
import Data.Either (lefts, rights)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )
import Text.Read (readMaybe)

type PotentialPassport = Map String String

parse :: [String] -> [PotentialPassport]
parse xs = p Map.empty [] xs
  where
    addToPassport :: PotentialPassport -> String -> PotentialPassport
    addToPassport p line = foldl (\m (k, v) -> Map.insert k v m) p kvs
      where
        pairs = splitOn " " line
        kv x = (k, v)
          where
            [k, v] = splitOn ":" x
        kvs = map kv pairs

    p :: PotentialPassport -> [PotentialPassport] -> [String] -> [PotentialPassport]
    p cur res [] = reverse (cur : res)
    p cur res ("" : xs) = p Map.empty (cur : res) xs
    p cur res (x : xs) = p cur' res xs
      where
        cur' = addToPassport cur x

data ValidationError
  = ExpectedInteger
  | ExpectedIntInRange (Int, Int)
  | InvalidPostfix
  | InvalidPrefix
  | ExpectedHexDigits Int
  | ExpectedElementIn (Set String)
  | ExpectedDigits Int
  | FieldMissing
  deriving (Eq, Show)

data FieldValidationError = FieldValidationError
  { fvaError :: ValidationError,
    fvaField :: String,
    fvaActual :: String
  }
  deriving (Eq, Show)

type PendingValidationError = String -> FieldValidationError

type FieldValidator a = String -> Either PendingValidationError a

fieldValidationError :: ValidationError -> String -> Either PendingValidationError a
fieldValidationError err actual = Left $ \field -> FieldValidationError {fvaError = err, fvaField = field, fvaActual = actual}

intValidator :: FieldValidator Int
intValidator xs =
  case readMaybe xs of
    Just x -> Right x
    Nothing -> fieldValidationError ExpectedInteger xs

intRangeValidator :: Int -> Int -> FieldValidator ()
intRangeValidator minInclusive maxInclusive s = do
  i <- intValidator s
  if i >= minInclusive && i <= maxInclusive
    then Right ()
    else fieldValidationError (ExpectedIntInRange (minInclusive, maxInclusive)) s

heightValidator :: FieldValidator ()
heightValidator s = do
  let (digits, rest) = span Char.isDigit s
  case rest of
    "cm" -> intRangeValidator 150 193 digits
    "in" -> intRangeValidator 59 76 digits
    _ -> fieldValidationError InvalidPostfix s

hexDigitsValidator :: Int -> FieldValidator ()
hexDigitsValidator expectedLength s =
  if lengthOk && noLeftover
    then Right ()
    else fieldValidationError (ExpectedHexDigits expectedLength) s
  where
    (hexdigits, rest) = span Char.isHexDigit s
    lengthOk = length hexdigits == expectedLength
    noLeftover = rest == ""

hairColorValidator :: FieldValidator ()
hairColorValidator [] = fieldValidationError FieldMissing ""
hairColorValidator (s : ss) =
  if s == '#'
    then hexDigitsValidator 6 ss
    else fieldValidationError InvalidPrefix (s : ss)

setMemberValidator :: Set String -> FieldValidator ()
setMemberValidator set s = if Set.member s set then Right () else fieldValidationError (ExpectedElementIn set) s

eyeColorValidator :: FieldValidator ()
eyeColorValidator = setMemberValidator eyeColors
  where
    eyeColors = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

digitsValidator :: Int -> FieldValidator ()
digitsValidator expectedLength s =
  if lengthOk && noLeftover
    then Right ()
    else fieldValidationError (ExpectedDigits expectedLength) s
  where
    (digits, rest) = span Char.isDigit s
    lengthOk = length digits == expectedLength
    noLeftover = rest == ""

passportIdValidator :: FieldValidator ()
passportIdValidator = digitsValidator 9

requiredFields :: Map String (FieldValidator ())
requiredFields =
  Map.fromList
    [ ("byr", intRangeValidator 1920 2002),
      ("iyr", intRangeValidator 2010 2020),
      ("eyr", intRangeValidator 2020 2030),
      ("hgt", heightValidator),
      ("hcl", hairColorValidator),
      ("ecl", eyeColorValidator),
      ("pid", passportIdValidator)
    ]

validatePassport :: PotentialPassport -> Either [FieldValidationError] ()
validatePassport p = if null errors then Right () else Left errors
  where
    validateField :: (String, FieldValidator ()) -> Either FieldValidationError ()
    validateField (field, validator) =
      case Map.lookup field p of
        Just v ->
          case validator v of
            Right _ -> Right ()
            Left fvaf -> Left $ fvaf field
        Nothing -> Left $ FieldValidationError {fvaError = FieldMissing, fvaField = field, fvaActual = ""}
    errors = validateField <$> Map.toList requiredFields |> lefts

countValidPassports :: [PotentialPassport] -> Int
countValidPassports ps = map validatePassport ps |> rights |> length

tests :: SpecWith ()
tests = do
  describe "countValidPassports" $
    it "works on example" $
      let input =
            [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
              "byr:1937 iyr:2017 cid:147 hgt:183cm",
              "",
              "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
              "hcl:#cfa07d byr:1929",
              "",
              "hcl:#ae17e1 iyr:2013",
              "eyr:2024",
              "ecl:brn pid:760753108 byr:1931",
              "hgt:179cm",
              "",
              "hcl:#cfa07d eyr:2025 pid:166559648",
              "iyr:2011 ecl:brn hgt:59in"
            ]
          parsed = parse input
       in countValidPassports parsed `shouldBe` 2
  describe "invalid passports" $
    it "are detected" $
      let input =
            [ "eyr:1972 cid:100",
              "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
              "",
              "iyr:2019",
              "hcl:#602927 eyr:1967 hgt:170cm",
              "ecl:grn pid:012533040 byr:1946",
              "",
              "hcl:dab227 iyr:2012",
              "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
              "",
              "hgt:59cm ecl:zzz",
              "eyr:2038 hcl:74454a iyr:2023",
              "pid:3556412378 byr:2007"
            ]
          parsed = parse input
          expectedErrors =
            [ Left
                [ FieldValidationError {fvaError = ExpectedIntInRange (2020, 2030), fvaField = "eyr", fvaActual = "1972"},
                  FieldValidationError {fvaError = InvalidPostfix, fvaField = "hgt", fvaActual = "170"},
                  FieldValidationError {fvaError = ExpectedDigits 9, fvaField = "pid", fvaActual = "186cm"}
                ],
              Left [FieldValidationError {fvaError = ExpectedIntInRange (2020, 2030), fvaField = "eyr", fvaActual = "1967"}],
              Left [FieldValidationError {fvaError = InvalidPrefix, fvaField = "hcl", fvaActual = "dab227"}],
              Left
                [ FieldValidationError {fvaError = ExpectedIntInRange (1920, 2002), fvaField = "byr", fvaActual = "2007"},
                  FieldValidationError {fvaError = ExpectedElementIn (Set.fromList ["amb", "blu", "brn", "grn", "gry", "hzl", "oth"]), fvaField = "ecl", fvaActual = "zzz"},
                  FieldValidationError {fvaError = ExpectedIntInRange (2020, 2030), fvaField = "eyr", fvaActual = "2038"},
                  FieldValidationError {fvaError = InvalidPrefix, fvaField = "hcl", fvaActual = "74454a"},
                  FieldValidationError {fvaError = ExpectedIntInRange (150, 193), fvaField = "hgt", fvaActual = "59"},
                  FieldValidationError {fvaError = ExpectedIntInRange (2010, 2020), fvaField = "iyr", fvaActual = "2023"},
                  FieldValidationError {fvaError = ExpectedDigits 9, fvaField = "pid", fvaActual = "3556412378"}
                ]
            ]
       in map validatePassport parsed `shouldBe` expectedErrors

main :: IO ()
main = do
  Common.aoc
    4
    Common.Solution
      { Common.parse = parse . lines,
        Common.test = const tests,
        Common.solution = countValidPassports
      }
