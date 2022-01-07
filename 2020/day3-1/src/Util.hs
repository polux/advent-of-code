-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Util where

import qualified Data.Text.Lazy as LT
import Debug.Trace ( trace )
import Text.Pretty.Simple (pShow)
import Text.Megaparsec (errorBundlePretty, runParser,  Parsec )
import Data.Void (Void)

pTraceShow :: Show a => a -> b -> b
pTraceShow x = trace (LT.unpack (pShow x))

pTraceShowId :: Show a => a -> a
pTraceShowId x = pTraceShow x x

type Parser = Parsec Void String

runParserOrDie :: Parser a -> String -> a
runParserOrDie p str =
  case runParser p "" str of
    Left err -> error (errorBundlePretty err)
    Right res -> res