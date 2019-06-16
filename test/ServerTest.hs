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

module ServerTest (serverTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai.Handler.Warp as Warp

import Data
import Server

test404 :: AppState -> Test
test404 app = TestLabel "test404" $ TestCase $ do
    Warp.withApplication (return server404Handler) $ \port ->  do
        let url = "http://127.0.0.1:" <> (textShow port) <> "/"
        let req = (parseRequest_ . unpack $ url)
        withResponse req (manager app) $ \resp -> do
            json <- httpResponseBodyJSON url resp 1024 :: IO Value
            assertEqual "code" 404 $ (fromRight' $ jsonGet "code" json :: Int)
            assertEqual "message" "Not Found" $ (fromRight' $ jsonGet "message" json :: Text)
            let HTTPTypes.Status st _ = Client.responseStatus resp
            assertEqual "status" 404 $ st
    return ()

testPing :: AppState -> Test
testPing app = TestLabel "testPing" $ TestCase $ do
    Warp.withApplication (return $ serverPingHandler app) $ \port ->  do
        let url = "http://127.0.0.1:" <> (textShow port) <> "/"
        let req = (parseRequest_ . unpack $ url)
        withResponse req (manager app) $ \resp -> do
            json <- httpResponseBodyJSON url resp 1024 :: IO Value
            assertEqual "code" 200 $ (fromRight' $ jsonGet "code" json :: Int)
            assertEqual "message" "pong" $ (fromRight' $ jsonGet "message" json :: Text)
            let HTTPTypes.Status st _ = Client.responseStatus resp
            assertEqual "status" 200 $ st
    return ()

testWebHook :: AppState -> Test
testWebHook app = TestLabel "testWebHook" $ TestCase $ do
    Warp.withApplication (return $ serverWebHookHandler app) $ \port ->  do
        let url = "http://127.0.0.1:" <> (textShow port) <> "/"
        -- get
        let reqGet = (parseRequest_ . unpack $ url)
                { Client.method = "GET"
                }
        stGet <- withResponse reqGet (manager app) $ \resp -> do
            let HTTPTypes.Status stGet _ = Client.responseStatus resp
            return stGet
        assertEqual "get" 404 $ stGet
        -- post
        wh <- readFile "test/data/webhook-issue-opened.json"
        let reqPost = (parseRequest_ . unpack $ url)
                { Client.method = "POST"
                , Client.requestBody = (Client.RequestBodyBS . encodeUtf8) wh
                }
        stPost <- withResponse reqPost (manager app) $ \resp -> do
            let HTTPTypes.Status stPost _ = Client.responseStatus resp
            return stPost
        assertEqual "post" 200 $ stPost

    return ()

serverTest :: AppState -> Test
serverTest app = TestLabel "ServerTest" $ TestList
    [ test404 app
    , testPing app
    , testWebHook app
    ]
