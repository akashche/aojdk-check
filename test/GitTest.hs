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

module GitTest (gitTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude
import qualified Data.Text as Text
import qualified System.Process as Process

import Git

test1 :: Test
test1 = TestLabel "test1" $ TestCase $ do
    let user = "akashche"
    token <- Text.strip <$> readFile ".secret/token.txt"
    let url = "https://" <> user <> ":" <> token <> "@github.com/akashche/push-test.git"

    Process.callProcess "/bin/rm" ["-rf", "work"]
    Process.callProcess "/bin/mkdir" ["work"]
    Process.callProcess "/usr/bin/git" ["init", "work/repo"]
    writeFile (unpack "work/repo/foo.txt") "foo\n"
    Process.callProcess "/usr/bin/git" ["--git-dir", "work/repo/.git", "--work-tree", "work/repo", "add", "*"]
    Process.callProcess "/usr/bin/git" ["--git-dir", "work/repo/.git", "--work-tree", "work/repo", "commit", "-am", "test1"]

    gitWithRepo "work/repo" $ \repo -> do

        dt <- getCurrentTime
        let secs = (floor . utcTimeToPOSIXSeconds) dt :: Int
        let branch = textShow secs

        gitCreateBranch repo "refs/heads/master" branch

        appendFile (unpack "work/repo/foo.txt") "bar\n"
        writeFile (unpack "work/repo/bar.txt") "bar\n"

        -- stage and commit
        let paths = fromList
                [ "foo.txt"
                , "bar.txt"
                ]
        gitCommitBranch repo branch paths "test" "test@test.org" "test msg"
        gitPushBranch repo branch url

    Process.callProcess "/usr/bin/git" ["--git-dir", "work/repo/.git", "--work-tree", "work/repo", "status"]
    return ()

gitTest :: Test
gitTest = TestLabel "GitTest" (TestList
    [ test1
    ])

