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

module PatchTest (patchTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude
import qualified Data.Vector as Vector

import Patch

testParse :: Test
testParse = TestLabel "testParse" $ TestCase $ do
    paths <- patchParse "test/data/jdk.patch"
    assertEqual "modified len" 1 $
        Vector.length (modified paths)
    assertEqual "modified" "new/src/windows/native/java/net/DualStackPlainDatagramSocketImpl.c" $
        (modified paths) ! 0
    assertEqual "added len" 1 $
        Vector.length (added paths)
    assertEqual "added" "new/test/java/net/DatagramSocket/B8035653.java" $
        (added paths) ! 0
    assertEqual "deleted len" 0 $
        Vector.length (deleted paths)
    return ()

patchTest :: Test
patchTest = TestLabel "PatchTest" (TestList
    [ testParse
    ])
