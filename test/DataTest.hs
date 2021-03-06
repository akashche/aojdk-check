--
-- Copyright 2019, Red Hat Inc.
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

module DataTest (dataTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude

import Data

testTokenExpiry :: Test
testTokenExpiry = TestLabel "testTokenExpiry" $ TestCase $ do
    let te = GitHubTokenExpiry "2019-01-28T22:58:28Z"
    let Right before = dateParseISO8601 "2019-01-28 22:58:27"
    let Right after = dateParseISO8601 "2019-01-28 22:58:29"
    assertBool "before" $ before < (fromJust $ githubTokenExpiryTime te)
    assertBool "after" $ after  > (fromJust $ githubTokenExpiryTime te)
    let ts = floor . utcTimeToPOSIXSeconds . fromJust . githubTokenExpiryTime $ te :: Int64
    let bs = floor . utcTimeToPOSIXSeconds $ before :: Int64
    assertBool "posix before" $ bs < ts
    assertBool "posix after" $ bs > (ts - 10)
    return ()

dataTest :: Test
dataTest = TestLabel "DataTest" (TestList
    [ testTokenExpiry
    ])
