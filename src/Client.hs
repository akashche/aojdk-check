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

module Client
    ( clientFetchWebrevPatch
    , clientCreatePullRequest
    , clientCreateCheck
    , clientUpdateCheck
    ) where

import Prelude ()
import VtUtils.Prelude

import Config

clientFetchWebrevPatch :: Config -> Text -> IO Text
clientFetchWebrevPatch _ _ = return "TODO"

clientCreatePullRequest :: Config -> IO Text
clientCreatePullRequest _ = return "TODO"

clientCreateCheck :: Config -> IO Text
clientCreateCheck _ = return "TODO"

clientUpdateCheck :: Config -> IO Text
clientUpdateCheck _ = return "TODO"
