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
    ( clientCreateManager
    , clientFetchWebrevPatch
    , clientAddIssueComment
    , clientCreatePullRequest
    , clientCreateCheck
    , clientUpdateCheck
    ) where

import Prelude ()
import VtUtils.Prelude
-- import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.OpenSSL as OpenSSL
import qualified OpenSSL.Session as OpenSSLSession

import App

-- todo: timeouts
clientCreateManager ::Config -> IO Manager
clientCreateManager _ =
    OpenSSL.withOpenSSL $
        newManager (OpenSSL.opensslManagerSettings OpenSSLSession.context)

clientFetchWebrevPatch :: App -> Text -> IO Text
clientFetchWebrevPatch app url = do
    let mb = get ((maxResponseSizeBytes . client . config) app :: MaxResponseSizeBytes)
    let req = (parseRequest_ . unpack) url
    withResponse req (manager app) $ \resp ->
        httpResponseBodyText url resp mb

clientAddIssueComment :: App -> Text -> IO Text
clientAddIssueComment _ _ = return "TODO"

clientCreatePullRequest :: App -> IO Text
clientCreatePullRequest _ = return "TODO"

clientCreateCheck :: App -> IO Text
clientCreateCheck _ = return "TODO"

clientUpdateCheck :: Config -> IO Text
clientUpdateCheck _ = return "TODO"
