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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Hg
    ( hgImportPatch
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.Text as Text
import qualified System.Directory as Directory

-- https://stackoverflow.com/q/17247538/314015

hgImportPatch :: Text -> Text -> Text -> Text -> IO ()
hgImportPatch exec repo patch out = do
    let args = fromList ["--repository", repo, "import", "--no-commit", patch]
    code <- processSpawnAndWait exec args out
    when (0 /= code) $ do
        exist <- Directory.doesFileExist (unpack out)
        output <- if exist then
            readFile (unpack out)
        else
            return ""
        (error . unpack) $ "Mercurial error,"
            <> " code: [" <> (textShow code) <>"],"
            <> " output: [" <> (Text.take 1024 (Text.strip output)) <> "]"
