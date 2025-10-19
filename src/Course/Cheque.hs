{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Distribution.Compat.Prelude (Show(show))

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq
showDigit3 :: Digit3 -> Chars
showDigit3 (D1 d) =showDigit d
showDigit3 (D2 Zero d1) = showDigit d1
showDigit3 (D2 One d1) =
  case d1 of
    Zero -> "ten"
    One -> "eleven"
    Two -> "twelve"
    Three -> "thirteen"
    Four -> "fourteen"
    Five -> "fifteen"
    Six -> "sixteen"
    Seven -> "seventeen"
    Eight -> "eighteen"
    Nine -> "nineteen"
showDigit3 (D2 d2 Zero) =
  case d2 of
    Two -> "twenty"
    Three -> "thirty"
    Four -> "forty"
    Five -> "fifty"
    Six -> "sixty"
    Seven -> "seventy"
    Eight -> "eighty"
    Nine -> "ninety"
showDigit3 (D2 d2 d1) = showDigit3 (D2 d2 Zero) ++ "-" ++ showDigit d1
showDigit3 (D3 Zero d2 d1) = showDigit3 (D2 d2 d1)
showDigit3 (D3 d3 Zero Zero) = showDigit d3 ++ " hundred"
showDigit3 (D3 d3 d2 d1) = showDigit d3 ++ " hundred and " ++ showDigit3 (D2 d2 d1)
-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars str =
  showDollar dollorsPart ++ " and " ++ showCents centsPart
  where
    (dollorsOriPart, centsOriPart) = splitOn (== '.') str

    dollorsPart = clearZero (
      foldr
        (\c acc ->
          case fromChar c of
            Empty -> acc
            Full d -> accumulateDigit d acc)
        Nil
        dollorsOriPart)
      where
        accumulateDigit :: Digit -> List Digit3 -> List Digit3
        accumulateDigit d Nil = D1 d :. Nil
        accumulateDigit d (D1 d1 :. ds) = D2 d d1 :. ds
        accumulateDigit d (D2 d2 d1 :. ds) = D3 d d2 d1 :. ds
        accumulateDigit d p = D1 d :. p
        clearZero :: List Digit3 -> List Digit3
        clearZero Nil = Nil
        clearZero (D1 Zero :. ds) = clearZero ds
        clearZero (D2 Zero d :. ds) = clearZero (D1 d :. ds)
        clearZero (D3 Zero d2 d1 :. ds) = clearZero (D2 d2 d1 :. ds)
        clearZero ds = ds
    centsPart =
      take 2 $
        foldr
          (\c acc ->
            case fromChar c of
              Empty -> acc
              Full d -> d :. acc)
          Nil
          centsOriPart

    showDollar :: List Digit3 -> Chars
    showDollar Nil = "zero dollars"
    showDollar (D1 One :. Nil) = "one dollar"
    showDollar ds =
      fst (foldr
          (\d (acc, illions) ->
            case illions of
              Nil -> error "illion list exhausted"
              (ill :. rest) ->
                (if isZero d
                  then acc
                  else
                    showDigit3 d
                    ++ (if ill == "" then "" else " " ++ ill)
                    ++ (if acc == "" then "" else " " ++ acc)
                , rest
                ))
          ("dollars", illion)
          ds)
      where
        isZero :: Digit3 -> Bool
        isZero (D1 Zero) = True
        isZero (D2 Zero Zero) = True
        isZero (D3 Zero Zero Zero) = True
        isZero _ = False

    showCents :: List Digit -> Chars
    showCents Nil = "zero cents"
    showCents (d1 :. Nil) = showDigit3 (D2 d1 Zero) ++ " cents"
    showCents (Zero :. One :. Nil) = "one cent"
    showCents (d1 :. d2 :. Nil) = showDigit3 (D2 d1 d2) ++ " cents"
    showCents _ = error "centsPart should have at most two digits"

foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ z Nil         = z
foldr f z (x :. xs)   = f x (foldr f z xs)
splitOn :: (a -> Bool) -> List a -> (List a , List a)
splitOn _ Nil = (Nil, Nil)
splitOn p (x :. xs)
  | p x       = (Nil, xs)
  | otherwise = let (l, r) = splitOn p xs
                in (x :. l, r)

