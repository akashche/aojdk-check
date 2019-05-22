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

module ClientTest (clientTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude
import qualified Data.IORef as IORef
-- import qualified Network.HTTP.Client as Client
-- import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai.Handler.Warp as Warp

import Client
--import Config
import Data
import Server

authHandler :: (IORef.IORef Int) -> Application
authHandler counter req respond = do
    count <- IORef.readIORef counter
    if 0 == count then do
        IORef.atomicModifyIORef' counter $ \val -> (val + 1, ())
        if "POST" == requestMethod req then do
            nowSecs <- (floor . utcTimeToPOSIXSeconds) <$> getCurrentTime :: IO Int
            let dt = posixSecondsToUTCTime $ fromIntegral $ nowSecs + 1200
            let hm = httpRequestHeadersMap req
            if (isJust $ lookup "User-Agent" hm )
                && (isJust $ lookup "Authorization" hm)
                && (isJust $ lookup "Accept" hm) then
                respond $ responseLBS HTTPTypes.status201 [httpContentTypeJSON] $
                    encodePretty $ GitHubToken
                        { token = GitHubTokenBody "v1.abf14674c5d40c2f7de6833980566fdf5b975b1a"
                        , expires_at = GitHubTokenExpiry $ dateFormat "%Y-%m-%dT%H:%M:%SZ" dt
                        }
            else do
                respond $ responseLBS HTTPTypes.status400 [httpContentTypeJSON] $
                    encodePretty $ RespMsg "Invalid request" 400 (httpRequestPath req)
        else
            respond $ responseLBS HTTPTypes.status400 [httpContentTypeJSON] $
                encodePretty $ RespMsg "Invalid method" 400 (httpRequestPath req)
    else
        respond $ responseLBS HTTPTypes.status400 [httpContentTypeJSON] $
            encodePretty $ RespMsg "Cache fail" 400 (httpRequestPath req)

fetchHandler :: Application
fetchHandler _ respond =
    respond $ responseLBS HTTPTypes.status201 [("Content-Type", "text/plain")] $ "foo"

creationHandler :: Application
creationHandler req respond =
    if "POST" == requestMethod req then do
--         tx <- httpRequestBodyText req
--         putStrLn $ tx
        respond $ responseLBS HTTPTypes.status201 [httpContentTypeJSON] $ "{}"
    else
        respond $ responseLBS HTTPTypes.status400 [httpContentTypeJSON] $
            encodePretty $ RespMsg "Invalid method" 400 (httpRequestPath req)

testAuth :: AppState -> Test
testAuth tapp = TestLabel "testAuth" $ TestCase $ do
    counter <- IORef.newIORef (0 :: Int)
    Warp.withApplication (return $ authHandler counter) $ \port ->  do
        let app = appStateCustomizePort tapp port
        GitHubToken {token=tok1} <- clientGitHubAuth app
        let GitHubTokenBody tb1 = tok1
        GitHubToken {token=tok2} <- clientGitHubAuth app
        let GitHubTokenBody tb2 = tok2
        assertEqual "same token" tb1 tb2

testFetch :: AppState -> Test
testFetch app = TestLabel "testFetch" $ TestCase $ do
    Warp.withApplication (return $ fetchHandler) $ \port ->  do
        let url = "http://127.0.0.1:" <> (textShow port) <> "/"
        res <- clientFetchPatch app (FetchURL url)
        assertEqual "fetch" "foo" $ res

testCreatePR :: AppState -> Test
testCreatePR tapp = TestLabel "testCreatePR" $ TestCase $ do
    Warp.withApplication (return $ creationHandler) $ \port ->  do
        let app = appStateCustomizePort tapp port
        let pl = GitHubRequestPR
                { title = "test title"
                , head = "test-branch-1"
                , base = "test-branch-2"
                , body = "test body"
                }
        clientCreatePullRequest app pl

testCreateCheck :: AppState -> Test
testCreateCheck tapp = TestLabel "testCreateCheck" $ TestCase $ do
    Warp.withApplication (return $ creationHandler) $ \port ->  do
        let app = appStateCustomizePort tapp port
        let pl = GitHubRequestCheck
                { name = "test check"
                , head_sha = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
                , details_url = "https://github.com/akashche"
                , external_id = "42"
                }
        clientCreateCheck app pl

clientTest :: AppState -> Test
clientTest app = TestLabel "ClientTest" $ TestList
    [ testAuth app
    , testFetch app
    , testCreatePR app
    , testCreateCheck app
    ]
