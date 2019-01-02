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

module ClientTest (clientTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude

import Client

test1 :: Test
test1 = TestLabel "test1" $ TestCase $ do
    man <- clientCreateManager
--     tx <- clientFetchWebrevPatch man "http://cr.openjdk.java.net/~akasko/jdk8u/8035653/webrev.00/jdk.patch"
    tx <- clientFetchWebrevPatch man "https://github.com/akashche/aojdk-check/commit/e14c27642514fc2fe08f528a8a1b6678c3ab95bf.patch"
    putStrLn $ tx
    return ()

clientTest :: Test
clientTest = TestLabel "ClientTest" (TestList
    [ test1
    ])

