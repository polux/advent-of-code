-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

-- #region language extensions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- #endregion

module Main where

-- #region imports

import Control.Lens (each, folded, isn't, traversed, (%~), (&), (.~), (?~), (^.), (^..), (^?))
import Control.Lens.Extras (is)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Generics (Generic)
import Linear (V2 (..))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import Text.Regex.PCRE ((=~))
import Util (pTraceShow, pTraceShowId)
import Data.List.Split (splitOn)
import Data.Char (isDigit)

-- #endregion

type Input = [Map String String]

type Output = Int

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> Input
parse str = map parsePassport (splitOn "\n\n" str)
 where parsePassport block = M.fromList (map parseEntry (words block))
       parseEntry entry = let [key, val] = splitOn ":" entry in (key, val)

requiredKeys = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

solve :: Input -> Output
solve input = length (filter valid input)
 where
   valid passport =
     requiredKeys `S.isSubsetOf` M.keysSet passport
     && validBirthYear (passport M.! "byr")
     && validIssueYear (passport M.! "iyr")
     && validExpirationYear (passport M.! "eyr")
     && validHeight (passport M.! "hgt")
     && validHairColor (passport M.! "hcl")
     && validEyeColor (passport M.! "ecl")
     && validPassportId (passport M.! "pid")
   validateDate minDate maxDate str
     | length str /= 4 = False
     | any (not . isDigit) str = False
     | otherwise = let n = read str in n >= minDate && n <= maxDate
   validBirthYear = validateDate 1920 2002
   validIssueYear = validateDate 2010 2020
   validExpirationYear = validateDate 2020 2030
   validHeight str =
     case parseHeight str of
       Just (n, "cm") -> n >= 150 && n <= 193
       Just (n, "in") -> n >= 59 && n <= 76
       _ -> False
   validHairColor str = str =~ "#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]"
   validEyeColor str = str `S.member` S.fromList ["amb","blu", "brn", "gry", "grn", "hzl", "oth"]
   validPassportId str = length str == 9 && all isDigit str


parseHeight :: String -> Maybe (Int, String)
parseHeight str =
  case str =~ "([0-9]*)(cm|in)" of
    [[_, num, unit]] -> Just (read num, unit)
    _ -> Nothing
