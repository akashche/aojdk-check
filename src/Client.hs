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

import Config

-- todo: move to config
maxResponseSizeBytes :: Int
maxResponseSizeBytes = 8192

-- todo: timeouts
clientCreateManager :: IO Manager
clientCreateManager =
    OpenSSL.withOpenSSL $
        newManager (OpenSSL.opensslManagerSettings OpenSSLSession.context)

clientFetchWebrevPatch :: Manager -> Text -> IO Text
clientFetchWebrevPatch man url = do
    req <- parseRequest (unpack url)
    withResponse req man $ \resp ->
        httpResponseBodyText url (responseBody resp) maxResponseSizeBytes

clientAddIssueComment :: Config -> Text -> IO Text
clientAddIssueComment _ _ = return "TODO"

clientCreatePullRequest :: Config -> IO Text
clientCreatePullRequest _ = return "TODO"

clientCreateCheck :: Config -> IO Text
clientCreateCheck _ = return "TODO"

clientUpdateCheck :: Config -> IO Text
clientUpdateCheck _ = return "TODO"
