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
import qualified Data.ByteString.Base64 as Base64
import qualified Network.HTTP.Client as Client
-- import qualified Network.HTTP.Types as HTTPTypes

import App
import JWT
-- import Client

test1 :: App -> Test
test1 app = TestLabel "test1" $ TestCase $ do
--     tx <- clientFetchWebrevPatch man "http://cr.openjdk.java.net/~akasko/jdk8u/8035653/webrev.00/jdk.patch"
--     tx <- clientFetchWebrevPatch app "https://github.com/akashche/aojdk-check/commit/e14c27642514fc2fe08f528a8a1b6678c3ab95bf.patch"
--     putStrLn $ tx
--     assertBool "non-empty" $
--         Text.length tx > 0
    let guser = get ((user . github . client . config) app :: GitHubUser)
    let gtoken = get (githubToken app :: GitHubToken)
    let comb = guser <> ":" <> gtoken
    let cbytes = encodeUtf8 comb
    let _base64 = Base64.encode cbytes
    let mb = get ((maxResponseSizeBytes . client . config) app :: MaxResponseSizeBytes)

    {--
    let url = "https://api.github.com/repos/akashche/webhook-test/issues/1"
    let req = ((parseRequest_ . unpack) url)
            { Client.method = "GET"
            , Client.requestHeaders =
                [ ("User-Agent", "Mozilla/5.0 (compatible; aojdkcheckbot/2.1; +https://github.com/akashche/aojdk-check)")
                , ("Authorization", "Basic " <> base64)
                ]
            }
    withResponse req (manager app) $ \resp -> do
        let HTTPTypes.Status st _ = Client.responseStatus resp
        assertEqual "issue" 200 $ st
        json <- httpResponseBodyJSON url resp mb :: IO Value
        putStrLn $ jsonEncodeText json
    --}

    {--
    tm <- getCurrentTime
    let url = "https://api.github.com/repos/akashche/webhook-test/issues/1/comments"
    let req = ((parseRequest_ . unpack) url)
            { Client.method = "POST"
            , Client.requestHeaders =
                [ ("User-Agent", "Mozilla/5.0 (compatible; aojdkcheckbot/2.1; +https://github.com/akashche/aojdk-check)")
                , ("Authorization", "Basic " <> base64)
                ]
            , Client.requestBody = (Client.RequestBodyLBS . encodePretty) $ object
                [ "body" .= dateFormatISO8601 tm
                ]
            }
    withResponse req (manager app) $ \resp -> do
        let HTTPTypes.Status st _ = Client.responseStatus resp
        assertEqual "comment" 201 $ st
        json <- httpResponseBodyJSON url resp mb :: IO Value
        putStrLn $ jsonEncodeText json
    --}

    _jwt <- jwtCreate ".secret/aojdk-check-test.2019-01-20.private-key.pem" "23953" 600

    {--
    let url = "https://api.github.com/repos/akashche/webhook-test/check-runs"
    let req = ((parseRequest_ . unpack) url)
            { Client.method = "POST"
            , Client.requestHeaders =
                [ ("User-Agent", "Mozilla/5.0 (compatible; aojdkcheckbot/2.1; +https://github.com/akashche/aojdk-check)")
                , ("Authorization", "Bearer " <> jwt)
                , ("Accept", "application/vnd.github.antiope-preview+json")
                ]
            , Client.requestBody = (Client.RequestBodyLBS . encodePretty) $ object
                [ "name" .= ("test1" :: Text)
                , "head_sha" .= ("ae64e3d2701f611f4a09b32e991f23aac5fd9f48" :: Text)
                , "details_url" .= ("https://github.com/akashche" :: Text)
                , "external_id" .= ("42" :: Text)
                ]
            }
    --}
    {---
    let url = "https://api.github.com/app/installations/602759/access_tokens"
    let req = ((parseRequest_ . unpack) url)
            { Client.method = "POST"
            , Client.requestHeaders =
                [ ("User-Agent", "Mozilla/5.0 (compatible; aojdkcheckbot/2.1; +https://github.com/akashche/aojdk-check)")
                , ("Authorization", "Bearer " <> jwt)
                , ("Accept", "application/vnd.github.machine-man-preview+json")
                ]
            }
    --}
    let url = "https://api.github.com/repos/akashche/webhook-test/check-runs"
    let req = ((parseRequest_ . unpack) url)
            { Client.method = "POST"
            , Client.requestHeaders =
                [ ("User-Agent", "Mozilla/5.0 (compatible; aojdkcheckbot/2.1; +https://github.com/akashche/aojdk-check)")
                , ("Authorization", "token " <> "v1.21ea9a0ed79fa0328667d6386b2f6b3327a16cf0")
                , ("Accept", "application/vnd.github.machine-man-preview+json")
                , ("Accept", "application/vnd.github.antiope-preview+json")
                ]
            , Client.requestBody = (Client.RequestBodyLBS . encodePretty) $ object
                [ "name" .= ("test1" :: Text)
                , "head_sha" .= ("ae64e3d2701f611f4a09b32e991f23aac5fd9f48" :: Text)
                , "details_url" .= ("https://github.com/akashche" :: Text)
                , "external_id" .= ("42" :: Text)
                ]
            }
    withResponse req (manager app) $ \resp -> do
        tx <- httpResponseBodyText url resp mb
        putStrLn $ tx
--         let HTTPTypes.Status st _ = Client.responseStatus resp
--         assertEqual "check" 200 $ st
--         json <- httpResponseBodyJSON url resp mb :: IO Value
--         putStrLn $ jsonEncodeText json



    return ()

clientTest :: App -> Test
clientTest app = TestLabel "ClientTest" $ TestList
    [ test1 app
    ]
