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

module Server
    ( Handler
    , RespMsg(..)
    , serverRun
    -- handlers
    , server404Handler
    , server500Handler
    , serverPingHandler
    , serverWebHookHandler
    ) where

import Prelude ()
import VtUtils.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai.Handler.Warp as Warp

import App
import WebHook

data RespMsg = RespMsg
    { message :: Text
    , code :: Int
    , path :: Text
    } deriving (Generic, Show)
instance ToJSON RespMsg

type Handler = Application

server404Handler ::Handler
server404Handler req respond = do
    let err = RespMsg "Not Found" 404 (httpRequestPath req)
    respond $ responseLBS HTTPTypes.status404 [httpContentTypeJSON] $
        encodePretty err

server500Handler :: SomeException -> Handler
server500Handler exc req respond = do
    let err = RespMsg (textShow exc) 500 (httpRequestPath req)
    respond $ responseLBS HTTPTypes.status500 [httpContentTypeJSON] $
        encodePretty err

serverPingHandler :: App -> Handler
serverPingHandler _ req respond = do
    let pong = RespMsg "pong" 200 (httpRequestPath req)
    respond $ responseLBS HTTPTypes.status200 [httpContentTypeJSON] $
        encodePretty pong

serverWebHookHandler :: App -> Handler
serverWebHookHandler app req respond = do
     if "POST" == requestMethod req then do
        val <- httpRequestBodyJSON req :: IO Value
        _ <- forkIO $ webHookReceive app val
        respond $ responseLBS HTTPTypes.status200 [] ""
     else do
        let err = RespMsg "Not Found" 404 (httpRequestPath req)
        respond $ responseLBS HTTPTypes.status404 [httpContentTypeJSON] $
            encodePretty err

handlers :: HashMap Text (App -> Handler)
handlers = HashMap.fromList
    [ ("/ping", serverPingHandler)
    , ("/webhooks", serverWebHookHandler)
    ]

master :: App -> Handler
master app req respond = do
    let reqpath = (decodeUtf8 . rawPathInfo) req
    case lookup reqpath handlers of
        Just ha -> do
            outcome <- try $ ha app req respond
            case outcome of
                Left exc -> server500Handler exc req respond
                Right rr -> return rr
        Nothing -> server404Handler req respond

serverRun :: App -> IO ()
serverRun app = do
    let ServerTcpPort port = tcpPort . server . config $ app
    let settings = Warp.setPort port Warp.defaultSettings
    Warp.runSettings settings (master app)
    return ()
