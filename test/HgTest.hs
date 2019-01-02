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

module HgTest (hgTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude
import qualified System.Process as Process

import Hg

testImportPatch :: Test
testImportPatch = TestLabel "testImportPatch" $ TestCase $ do
    Process.callProcess "/bin/rm" ["-rf", "work"]
    Process.callProcess "/bin/mkdir" ["work"]
    Process.callProcess "/usr/bin/hg" ["init", "work/hgrepo"]
    writeFile (unpack "work/hgrepo/foo.txt") "foo\n"
    Process.callProcess "/usr/bin/hg" ["--cwd", "work/hgrepo", "add", "foo.txt"]
    Process.callProcess "/usr/bin/hg" ["--repository", "work/hgrepo", "commit", "-m", "test"]
    Process.callProcess "/usr/bin/hg" ["--repository", "work/hgrepo", "status"]
    hgImportPatch "/usr/bin/hg" "work/hgrepo" "test/data/hg.diff" "work/out.txt"
    Process.callProcess "/usr/bin/hg" ["--repository", "work/hgrepo", "status"]
    return ()

hgTest :: Test
hgTest = TestLabel "HgTest" (TestList
    [ testImportPatch
    ])

