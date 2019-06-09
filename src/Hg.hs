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
    ( HgRepository(..)
    , HgExecutable(..)
    , HgOutputPath(..)
    , HgImportPatchException(..)
    , hgImportPatch
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.Text as Text
import qualified System.Directory as Directory

newtype HgRepository = HgRepository Text
    deriving Show

newtype HgExecutable = HgExecutable Text
    deriving Show

-- todo: newtype HgPatch

newtype HgOutputPath = HgOutputPath Text
    deriving Show

data HgImportPatchException = HgImportPatchException
    { executable :: HgExecutable
    , repository :: HgRepository
    , outputCode :: Int
    , outputText :: Text
    }
instance Exception HgImportPatchException
instance Show HgImportPatchException where
    show e@(HgImportPatchException {executable, repository, outputCode, outputText}) = errorShow e $
               "Mercurial error,"
            <> " executable: [" <> (textShow executable) <> "],"
            <> " repository: [" <> (textShow repository) <> "],"
            <> " code: [" <> (textShow outputCode) <> "],"
            <> " message: [" <> (Text.take 1024 (Text.strip outputText)) <> "]"

-- https://stackoverflow.com/q/17247538/314015

hgImportPatch :: HgExecutable -> HgRepository -> Text -> HgOutputPath -> IO ()
hgImportPatch exec'@(HgExecutable exec) repo'@(HgRepository repo) patch (HgOutputPath out) = do
    let args = fromList ["--repository", repo, "import", "--no-commit", patch]
    code <- processSpawnAndWait exec args out
    when (0 /= code) $ do
        exist <- Directory.doesFileExist (unpack out)
        output <- if exist then
            readFile (unpack out)
        else
            return ""
        throwIO $ HgImportPatchException exec' repo' code output
