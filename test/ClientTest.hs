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
-- import qualified Network.HTTP.Client as Client
-- import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai.Handler.Warp as Warp

import App
import Client
import Data
import Server

authHandler :: Handler
authHandler req respond = do
     if "POST" == requestMethod req then do
        let hm = httpRequestHeadersMap req
        if (isJust $ lookup "User-Agent" hm )
            && (isJust $ lookup "Authorization" hm)
            && (isJust $ lookup "Accept" hm) then
            respond $ responseLBS HTTPTypes.status201 [httpContentTypeJSON] $
                encodePretty $ GitHubToken
                    { token = GitHubTokenBody "v1.abf14674c5d40c2f7de6833980566fdf5b975b1a"
                    , expires_at = GitHubTokenExpiry "2019-01-28T22:58:28Z"
                    }
        else do
            respond $ responseLBS HTTPTypes.status400 [httpContentTypeJSON] $
                encodePretty $ RespMsg "Invalid request" 400 (httpRequestPath req)
     else
        respond $ responseLBS HTTPTypes.status400 [httpContentTypeJSON] $
            encodePretty $ RespMsg "Invalid method" 400 (httpRequestPath req)

testAuth :: App -> Test
testAuth app = TestLabel "testAuth" $ TestCase $ do
    Warp.withApplication (return $ authHandler) $ \port ->  do
        let gcf = (github . config $ app)
                { urlAuth = GitHubUrlAuth $
                    textFormat (getText . urlAuth . github . config $ app) $
                        fromList [(textShow port), "{}"]
                }
        tok <- clientGitHubAuth (manager app) (client . config $ app) gcf
        putStrLn $ textShow tok

--     tx <- clientFetchWebrevPatch man "http://cr.openjdk.java.net/~akasko/jdk8u/8035653/webrev.00/jdk.patch"
--     tx <- clientFetchWebrevPatch app "https://github.com/akashche/aojdk-check/commit/e14c27642514fc2fe08f528a8a1b6678c3ab95bf.patch"
--     putStrLn $ tx
--     assertBool "non-empty" $
--         Text.length tx > 0
--     let agent = "Mozilla/5.0 (compatible; aojdkcheckbot/2.1; +https://github.com/akashche/aojdk-check)"
--     let mb = get ((maxResponseSizeBytes . client . config) app :: MaxResponseSizeBytes)
--     token <- clientGithubAuth app ".secret/aojdk-check-test.2019-01-20.private-key.pem"
--     putStrLn $ token
--     let token = "v1.78cb78537df05374770ccae40c77889ad25a8ec6"

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

clientTest :: App -> Test
clientTest app = TestLabel "ClientTest" $ TestList
    [ testAuth app
    ]
