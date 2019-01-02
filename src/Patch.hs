--
-- Copyright 2018, Red Hat Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Patch
    ( PatchPaths(..)
    , patchParse
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Text.Parsec as Parsec

data PatchPaths = PatchPaths
    { modified :: Vector Text
    , added :: Vector Text
    , deleted :: Vector Text
    } deriving Show

data Path =
      Modified Text
    | Added Text
    | Deleted Text

pathOnly :: Parser Text
pathOnly = do
    _ <- (Parsec.string "---") <|> (Parsec.string "+++")
    parsecWhitespace
    pack <$> Parsec.manyTill Parsec.anyChar (Parsec.char '\t')

oneLine :: Parser Path
oneLine = do
    minus <- parsecParseText pathOnly <$> parsecLinePrefix "---"
    plus <- parsecParseText pathOnly <$> parsecLinePrefix "+++"
    if "/dev/null" == minus then
        return (Added plus)
    else if "/dev/null" == plus then
        return (Deleted minus)
    else
        return (Modified plus)

paths :: Parser PatchPaths
paths = do
    (ms, ns, ds) <- scan
    return $ PatchPaths (fromList ms) (fromList ns) (fromList ds)
    where
        scan = recur <|> (return ([], [], []))
        recur = do
            x <- parsecTry oneLine
            (ms, ns, ds) <- scan
            case x of
                Modified t -> return (t:ms, ns, ds)
                Added t -> return (ms, t:ns, ds)
                Deleted t -> return (ms, ns, t:ds)

patchParse :: Text -> IO PatchPaths
patchParse file =
     parsecParseFile paths file

