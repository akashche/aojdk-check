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
    ( RespMsg(..)
    , runServer
    ) where

import Prelude ()
import VtUtils.Prelude

import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.HashMap.Strict as HashMap
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai.Handler.Warp as Warp

import Config
import WebHooks

data RespMsg = RespMsg
    { message :: Text
    , code :: Int
    , path :: Text
    } deriving (Generic, Show)
instance ToJSON RespMsg

a404Handler :: Application
a404Handler req respond = do
    let err = RespMsg "Not Found" 404 (httpRequestPath req)
    respond $ responseLBS HTTPTypes.status404 [httpContentTypeJSON] $
        AesonPretty.encodePretty err

a500Handler :: SomeException -> Application
a500Handler exc req respond = do
    let err = RespMsg (textShow exc) 500 (httpRequestPath req)
    respond $ responseLBS HTTPTypes.status500 [httpContentTypeJSON] $
        AesonPretty.encodePretty err

pingHandler :: Config -> Application
pingHandler _ req respond = do
    let pong = RespMsg "pong" 200 (httpRequestPath req)
    respond $ responseLBS HTTPTypes.status200 [httpContentTypeJSON] $
        AesonPretty.encodePretty pong

webhooksHandler :: Config -> Application
webhooksHandler cf req respond = do
     if "POST" == requestMethod req then do
        val <- httpRequestBodyJSON req :: IO Value
        _ <- forkIO $ receiveWebHook cf val
        respond $ responseLBS HTTPTypes.status200 [] ""
     else do
        let err = RespMsg "Not Found" 404 (httpRequestPath req)
        respond $ responseLBS HTTPTypes.status404 [httpContentTypeJSON] $
            AesonPretty.encodePretty err

handlers :: HashMap Text (Config -> Application)
handlers = HashMap.fromList
    [ ("/ping", pingHandler)
    , ("/webhooks", webhooksHandler)
    ]

application :: Config -> Application
application cf req respond = do
    let reqpath = (decodeUtf8 . rawPathInfo) req
    case lookup reqpath handlers of
        Just ha -> do
            outcome <- try $ ha cf req respond
            case outcome of
                Left exc -> a500Handler exc req respond
                Right rr -> return rr
        Nothing -> a404Handler req respond

runServer :: Config -> IO ()
runServer cf = do
    let scf = server cf
    let settings = Warp.setPort (tcpPort scf) Warp.defaultSettings
    Warp.runSettings settings (application cf)
    return ()
