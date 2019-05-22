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

module Client
    ( clientCreateManager
    , ClientGitHubAuthException
    , clientGitHubAuth
    , clientFetchPatch
    , ClientCreatePullRequestException
    , clientCreatePullRequest
    , ClientCreateCheckException
    , clientCreateCheck
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.OpenSSL as OpenSSL
import qualified Network.HTTP.Types as HTTPTypes
import qualified Data.IORef as IORef
import qualified OpenSSL.Session as OpenSSLSession

import Config
import Data
import Digest

clientCreateManager :: Config -> IO Manager
clientCreateManager cf =
    let
        Config {client} = cf
        ClientConfig {maxCachedConnectionsPerHost, maxIdleConnections, maxResponseTimeoutMillis} = client
    in
        OpenSSL.withOpenSSL $ newManager $
            createContext maxCachedConnectionsPerHost maxIdleConnections maxResponseTimeoutMillis
    where
        createContext (ClientMaxCachedConnectionsPerHost mc) (ClientMaxIdleConnections mi) (ClientMaxResponseTimeout rt) =
            (OpenSSL.opensslManagerSettings OpenSSLSession.context)
                { Client.managerConnCount = mc
                , Client.managerIdleConnectionCount = mi
                , Client.managerResponseTimeout = Client.responseTimeoutMicro $ 1000 * rt
                }


data ClientGitHubAuthException = ClientGitHubAuthException
    { request :: Client.Request
    , status :: Int
    , response :: Text
    }
instance Exception ClientGitHubAuthException
instance Show ClientGitHubAuthException where
    show e@(ClientGitHubAuthException {request, status, response}) = errorShow e $
               "Error obtaining GitHub token,"
            <> " req: [" <> (textShow request) <> "],"
            <> " status: [" <> (textShow status) <> "],"
            <> " resp: [" <> (Text.take 1024 response) <> "]"

clientGitHubAuth :: AppState -> IO GitHubToken
clientGitHubAuth app = do
    let AppState {config, manager, githubToken} = app
    let Config {client, github} = config
    let ClientConfig {userAgent, maxResponseSizeBytes} = client
    let GitHubConfig {appId, appInstallId, keyPath, jwtDurationSecs, tokenMinRemainingSecs, customPort, urlAuth} = github
    let GitHubTokenHolder {tokenRef} = githubToken

    MVar.withMVar (lock githubToken) $ \_ -> do
        tok <- IORef.readIORef $ tokenRef
        nexp <- tokenNotExpired tok tokenMinRemainingSecs
        if nexp then
            return tok
        else do -- slow path
            jwt <- createJWT keyPath appId jwtDurationSecs
            let req = createReq jwt urlAuth appInstallId customPort userAgent
            ntok <- sendReq manager req maxResponseSizeBytes
            IORef.writeIORef tokenRef ntok
            return ntok
    where
        tokenNotExpired GitHubToken {expires_at} (GitHubTokenMinRemainingSecs min) = do
            now <- (floor . utcTimeToPOSIXSeconds) <$> getCurrentTime
            let exp = floor . utcTimeToPOSIXSeconds . getTime $ expires_at
            return $ exp - min > now

        createJWT (GitHubKeyPath key) (GitHubAppId iss) (GitHubJWTDurationSecs dur) = do
            now <- (floor . utcTimeToPOSIXSeconds) <$> getCurrentTime
            let header = object
                    [ "alg" .= ("RS256" :: Text)
                    , "typ" .= ("JWT" :: Text)
                    ]
            let bod = object
                    [ "iat" .= now
                    , "exp" .= (now + dur)
                    , "iss" .= iss
                    ]
            let base64Json = Base64URL.encode . ByteStringLazy.toStrict . encodePretty
            let bs = (base64Json header) <> "." <> (base64Json bod)
            sign <- Base64URL.encode <$> digestSignRS256 key bs
            return $ JSONWebToken $ ByteString.concat [bs, ".", sign]

        createReq (JSONWebToken jwt) (GitHubUrlAuth aurl) (GitHubAppInstallId iid)
                (GitHubCustomPort port) (ClientUserAgent ua) =
            let url = textFormat aurl $ fromList [port, iid] in
            (parseRequest_ . unpack $ url)
                { Client.method = "POST"
                , Client.requestHeaders =
                    [ ("User-Agent", (encodeUtf8 ua))
                    , ("Authorization", "Bearer " <> jwt)
                    , ("Accept", "application/vnd.github.machine-man-preview+json")
                    ]
                }

        sendReq manager req (ClientMaxResponseSizeBytes mb) = do
            withResponse req manager $ \resp -> do
                let label = textShow req
                let HTTPTypes.Status st _ = Client.responseStatus resp
                when (201 /= st) $ do
                    tx <- httpResponseBodyText label resp mb
                    throwIO $ ClientGitHubAuthException req st tx
                httpResponseBodyJSON label resp mb :: IO GitHubToken


clientFetchPatch :: AppState -> FetchURL -> IO Text
clientFetchPatch app furl = do
    let AppState {config, manager} = app
    let Config {client} = config
    let ClientConfig {userAgent, maxResponseSizeBytes} = client
    let req = createReq furl userAgent
    sendReq manager req maxResponseSizeBytes
    where
        createReq (FetchURL url) (ClientUserAgent ua) =
            (parseRequest_ . unpack $ url)
                { Client.method = "GET"
                , Client.requestHeaders =
                    [ ("User-Agent", (encodeUtf8 ua))
                    ]
                }
        sendReq man req (ClientMaxResponseSizeBytes mb) =
            withResponse req man $ \resp ->
                httpResponseBodyText (textShow req) resp mb


data ClientCreatePullRequestException = ClientCreatePullRequestException
    { request :: Client.Request
    , status :: Int
    , response :: Text
    }
instance Exception ClientCreatePullRequestException
instance Show ClientCreatePullRequestException where
    show e@(ClientCreatePullRequestException {request, status, response}) = errorShow e $
               "Error creating pull request,"
            <> " req: [" <> (textShow request) <> "],"
            <> " status: [" <> (textShow status) <> "],"
            <> " resp: [" <> (Text.take 1024 response) <> "]"

clientCreatePullRequest :: AppState -> GitHubRequestPR -> IO ()
clientCreatePullRequest app pr = do
    let AppState {config, manager} = app
    let Config {client, github} = config
    let ClientConfig {userAgent, maxResponseSizeBytes} = client
    let GitHubConfig {accountName, repoName, urlCreatePullRequest, customPort} = github
    GitHubToken {token} <- clientGitHubAuth app
    let req = createReq userAgent urlCreatePullRequest accountName repoName customPort token
    sendReq manager req maxResponseSizeBytes
    where
        createReq (ClientUserAgent ua) (GitHubUrlCreatePullRequest prurl) (GitHubAccountName an)
                (GitHubRepoName rn) (GitHubCustomPort port) (GitHubTokenBody tok) =
            let url = textFormat prurl $ fromList [port, an, rn] in
            (parseRequest_ . unpack $ url)
                    { Client.method = "POST"
                    , Client.requestHeaders =
                        [ ("User-Agent", (encodeUtf8 ua))
                        , ("Authorization", "token " <> (encodeUtf8 tok))
                        ]
                    , Client.requestBody = Client.RequestBodyLBS . encodePretty $ pr
                    }
        sendReq man req (ClientMaxResponseSizeBytes mb) =
            withResponse req man $ \resp -> do
                let HTTPTypes.Status st _ = Client.responseStatus resp
                when (201 /= st) $ do
                    tx <- httpResponseBodyText (textShow req) resp mb
                    throwIO $ ClientCreatePullRequestException req st tx


data ClientCreateCheckException = ClientCreateCheckException
    { request :: Client.Request
    , status :: Int
    , response :: Text
    }
instance Exception ClientCreateCheckException
instance Show ClientCreateCheckException where
    show e@(ClientCreateCheckException {request, status, response}) = errorShow e $
               "Error creating check,"
            <> " req: [" <> (textShow request) <> "],"
            <> " status: [" <> (textShow status) <> "],"
            <> " resp: [" <> (Text.take 1024 response) <> "]"
clientCreateCheck :: AppState -> GitHubRequestCheck -> IO ()
clientCreateCheck app check = do
    let AppState {config, manager} = app
    let Config {client, github} = config
    let ClientConfig {userAgent, maxResponseSizeBytes} = client
    let GitHubConfig {accountName, repoName, urlCreateCheck, customPort} = github
    GitHubToken {token} <- clientGitHubAuth app
    let req = createReq userAgent urlCreateCheck accountName repoName customPort token
    sendReq manager req maxResponseSizeBytes
    where
        createReq (ClientUserAgent ua) (GitHubUrlCreateCheck curl) (GitHubAccountName an)
                (GitHubRepoName rn) (GitHubCustomPort port) (GitHubTokenBody tok) =
            let url = textFormat curl $ fromList [port, an, rn] in
            (parseRequest_ . unpack $ url)
                { Client.method = "POST"
                , Client.requestHeaders =
                    [ ("User-Agent", (encodeUtf8 ua))
                    , ("Authorization", "token " <> (encodeUtf8 tok))
                    , ("Accept", "application/vnd.github.machine-man-preview+json")
                    , ("Accept", "application/vnd.github.antiope-preview+json")
                    ]
                , Client.requestBody = (Client.RequestBodyLBS . encodePretty) $ check
                }
        sendReq man req (ClientMaxResponseSizeBytes mb) =
            withResponse req man $ \resp -> do
                let HTTPTypes.Status st _ = Client.responseStatus resp
                when (201 /= st) $ do
                    tx <- httpResponseBodyText (textShow req) resp mb
                    throwIO $ ClientCreateCheckException req st tx
