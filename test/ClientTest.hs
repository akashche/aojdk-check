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
import qualified Data.IORef as IORef
-- import qualified Network.HTTP.Client as Client
-- import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai.Handler.Warp as Warp

import Client
import Config
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


testAuth :: AppState -> Test
testAuth app = TestLabel "testAuth" $ TestCase $ do
    counter <- IORef.newIORef (0 :: Int)
    Warp.withApplication (return $ authHandler counter) $ \port ->  do
        let gcf = (github . config $ app)
                { urlAuth = GitHubUrlAuth $
                    textFormat (getText . urlAuth . github . config $ app) $
                        fromList [(textShow port), "{}"]
                }
        tok1 <- clientGitHubAuth (manager app) (client . config $ app) gcf (githubToken app)
        tok2 <- clientGitHubAuth (manager app) (client . config $ app) gcf (githubToken app)
        assertEqual "same token" ((getText . token) tok1) ((getText . token) tok2)

testFetch :: AppState -> Test
testFetch app = TestLabel "testFetch" $ TestCase $ do
    Warp.withApplication (return $ fetchHandler) $ \port ->  do
        let url = "http://127.0.0.1:" <> (textShow port) <> "/"
        res <- clientFetchPatch (manager app) (client . config $ app) (FetchURL url)
        assertEqual "fetch" "foo" $ res


    -- PR
    {--
    let url = "https://api.github.com/repos/akashche/pr-checks-test/pulls"
    let req = ((parseRequest_ . unpack) url)
            { Client.method = "POST"
            , Client.requestHeaders =
                [ ("User-Agent", agent)
                , ("Authorization", "token " <> token)
                ]
            , Client.requestBody = (Client.RequestBodyLBS . encodePretty) $ object
                [ "title" .= ("Testing request 1" :: Text)
                , "head" .= ("test-1" :: Text)
                , "base" .= ("master" :: Text)
                , "body" .= ("Request 1 body text" :: Text)
                ]
            }
    withResponse req (manager app) $ \resp -> do
        tx <- httpResponseBodyText url resp mb
        putStrLn $ tx
    --}

    -- Check
    {--
    let url = "https://api.github.com/repos/akashche/pr-checks-test/check-runs"
    let req = ((parseRequest_ . unpack) url)
            { Client.method = "POST"
            , Client.requestHeaders =
                [ ("User-Agent", agent)
                , ("Authorization", "token " <> token)
                , ("Accept", "application/vnd.github.machine-man-preview+json")
                , ("Accept", "application/vnd.github.antiope-preview+json")
                ]
            , Client.requestBody = (Client.RequestBodyLBS . encodePretty) $ object
                [ "name" .= ("test-check-3" :: Text)
                , "head_sha" .= ("a816b4f12e843ed8bb00c2adf60cc5dd3757c221" :: Text)
                , "details_url" .= ("https://github.com/akashche" :: Text)
                , "external_id" .= ("44" :: Text)
                ]
            }
    withResponse req (manager app) $ \resp -> do
--         tx <- httpResponseBodyText url resp mb
--         putStrLn $ tx
--         let HTTPTypes.Status st _ = Client.responseStatus resp
--         assertEqual "check" 200 $ st
        json <- httpResponseBodyJSON url resp mb :: IO Value
        putStrLn $ jsonEncodeText json
    --}



    return ()

clientTest :: AppState -> Test
clientTest app = TestLabel "ClientTest" $ TestList
    [ testAuth app
    , testFetch app
    ]
