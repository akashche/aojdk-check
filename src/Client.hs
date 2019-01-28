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
    , clientGitHubAuth
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.OpenSSL as OpenSSL
import qualified Network.HTTP.Types as HTTPTypes
import qualified OpenSSL.Session as OpenSSLSession

import App
import Data
import Digest

createJWT :: GitHubKeyPath -> GitHubAppId -> JWTDurationSecs -> IO JSONWebToken
createJWT key iss dur = do
    now <- (floor . utcTimeToPOSIXSeconds) <$> getCurrentTime
    let header = object
            [ "alg" .= ("RS256" :: Text)
            , "typ" .= ("JWT" :: Text)
            ]
    let body = object
            [ "iat" .= now
            , "exp" .= (now + (getInt dur))
            , "iss" .= (getText iss)
            ]
    let base64Json = Base64URL.encode . ByteStringLazy.toStrict . encodePretty
    let bs = (base64Json header) <> "." <> (base64Json body)
    sign <- Base64URL.encode <$> digestSignRS256 (getText key) bs
    return $ JSONWebToken $ ByteString.concat [bs, ".", sign]

-- todo: timeouts
clientCreateManager ::Config -> IO Manager
clientCreateManager _ =
    OpenSSL.withOpenSSL $
        newManager (OpenSSL.opensslManagerSettings OpenSSLSession.context)

-- todo: fix args
-- clientFetchWebrevPatch :: App -> Text -> IO Text
-- clientFetchWebrevPatch app url = do
--     let mb = get ((maxResponseSizeBytes . client . config) app :: MaxResponseSizeBytes)
--     let req = (parseRequest_ . unpack) url
--     withResponse req (manager app) $ \resp ->
--         httpResponseBodyText url resp mb

-- todo : ioref
clientGitHubAuth :: Manager -> ClientConfig -> GitHubConfig -> IO GitHubToken
clientGitHubAuth man ccf gcf = do
    jwt <- createJWT (keyPath gcf) (appId gcf) (jwtDurationSecs gcf)
    let url = textFormat (getText . urlAuth $ gcf) $ fromList [getText . appInstallId $ gcf]
    let req = ((parseRequest_ . unpack) url)
            { Client.method = "POST"
            , Client.requestHeaders =
                [ ("User-Agent", (getBS . userAgent $ ccf))
                , ("Authorization", "Bearer " <> (getBS jwt))
                , ("Accept", "application/vnd.github.machine-man-preview+json")
                ]
            }
    withResponse req man $ \resp -> do
        let HTTPTypes.Status st _ = Client.responseStatus resp
        when (201 /= st) $ error "Token error" -- todo: details
        httpResponseBodyJSON url resp (getInt . maxResponseSizeBytes $ ccf)

