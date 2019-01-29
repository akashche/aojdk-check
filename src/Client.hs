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
    , clientFetchPatch
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.OpenSSL as OpenSSL
import qualified Network.HTTP.Types as HTTPTypes
import qualified Data.IORef as IORef
import qualified OpenSSL.Session as OpenSSLSession

import Config
import Data
import Digest

createJWT :: GitHubKeyPath -> GitHubAppId -> GitHubJWTDurationSecs -> IO JSONWebToken
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

clientCreateManager ::Config -> IO Manager
clientCreateManager cf =
    OpenSSL.withOpenSSL $ newManager $
        (OpenSSL.opensslManagerSettings OpenSSLSession.context)
            { Client.managerConnCount = getInt . maxCachedConnectionsPerHost . client $ cf
            , Client.managerIdleConnectionCount = getInt . maxIdleConnections . client $ cf
            , Client.managerResponseTimeout = Client.responseTimeoutMicro $
                1000 * (getInt . maxResponseTimeoutMillis . client $ cf)
            }

clientGitHubAuth :: Manager -> ClientConfig -> GitHubConfig -> GitHubTokenHolder -> IO GitHubToken
clientGitHubAuth man ccf gcf th = do
    tok <- IORef.readIORef $ tokenRef th
    now <- (floor . utcTimeToPOSIXSeconds) <$> getCurrentTime
    let exp = floor . utcTimeToPOSIXSeconds . getTime . expires_at $ tok
    let min = getInt . tokenMinRemainingSecs $ gcf
    if exp - min > now then do
        return tok
    else
        -- slow path
        MVar.withMVar (lock th) $ \_ -> do
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
            ntok <- withResponse req man $ \resp -> do
                let mb = getInt . maxResponseSizeBytes $ ccf
                let HTTPTypes.Status st _ = Client.responseStatus resp
                when (201 /= st) $ do
                    tx <- httpResponseBodyText url resp mb
                    error . unpack $
                           "Error obtaining GitHub token,"
                        <> " status: [" <> (textShow st) <> "],"
                        <> " resp: [" <> tx <> "]"
                httpResponseBodyJSON url resp mb :: IO GitHubToken
            IORef.atomicWriteIORef (tokenRef th) ntok
            return ntok

clientFetchPatch :: Manager -> ClientConfig -> FetchURL -> IO Text
clientFetchPatch man ccf url = do
    let req = (parseRequest_ . getString $ url)
            { Client.method = "GET"
            , Client.requestHeaders =
                [ ("User-Agent", (getBS . userAgent $ ccf))
                ]
            }
    let mb = getInt . maxResponseSizeBytes $ ccf
    withResponse req man $ \resp ->
        httpResponseBodyText (getText url) resp mb

