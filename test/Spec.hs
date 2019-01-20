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

import Prelude ()
import VtUtils.Prelude
import VtUtils.HUnit

import App
import Client

import ClientTest
import DigestTest
import GitTest
import HgTest
import JWTTest
import PatchTest
import ServerTest

main :: IO ()
main = do
    cf <- jsonDecodeFile "test/data/config-test.json"
    man <- clientCreateManager cf
    let tpath = get ((tokenFilePath . github . client) cf :: TokenFilePath)
    token <- readFile (unpack tpath)
    let app = App cf man (GitHubToken token)
    hunitMain $ fromList
        [ serverTest app
        , clientTest app
        , digestTest
        , gitTest
        , hgTest
        , jwtTest
        , patchTest
        ]
