--
-- Copyright 2019, Red Hat Inc.
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

module Config
    ( Config(..)
    -- server
    , ServerConfig(..)
    , ServerTcpPort(..)
    , ServerWebHookActionsEnabled(..)
    -- client
    , ClientConfig(..)
    , ClientUserAgent(..)
    , ClientMaxResponseSizeBytes(..)
    , ClientMaxCachedConnectionsPerHost(..)
    , ClientMaxIdleConnections(..)
    , ClientMaxResponseTimeout(..)
    -- github
    , GitHubConfig(..)
    , GitHubAppId(..)
    , GitHubAppInstallId(..)
    , GitHubAccountName(..)
    , GitHubRepoName(..)
    , GitHubKeyPath(..)
    , GitHubJWTDurationSecs(..)
    , GitHubTokenMinRemainingSecs(..)
    , GitHubCustomPort(..)
    , GitHubUrlAuth(..)
    , GitHubUrlCreatePullRequest(..)
    , GitHubUrlCreateCheck(..)
    , GitHubUrlAddComment(..)
    ) where

import Prelude ()
import VtUtils.Prelude

data Config = Config
    { server :: ServerConfig
    , client :: ClientConfig
    , github :: GitHubConfig
    } deriving (Generic, Show)
instance FromJSON Config

-- server

data ServerConfig = ServerConfig
    { tcpPort :: ServerTcpPort
    , webHookActionsEnabled :: ServerWebHookActionsEnabled
    } deriving (Generic, Show)
instance FromJSON ServerConfig

newtype ServerTcpPort = ServerTcpPort Int
    deriving (Generic, Show)
instance FromJSON ServerTcpPort

newtype ServerWebHookActionsEnabled = ServerWebHookActionsEnabled Bool
    deriving (Generic, Show)
instance FromJSON ServerWebHookActionsEnabled

-- client

data ClientConfig = ClientConfig
    { userAgent :: ClientUserAgent
    , maxResponseSizeBytes :: ClientMaxResponseSizeBytes
    , maxCachedConnectionsPerHost :: ClientMaxCachedConnectionsPerHost
    , maxIdleConnections :: ClientMaxIdleConnections
    , maxResponseTimeoutMillis :: ClientMaxResponseTimeout
    } deriving (Generic, Show)
instance FromJSON ClientConfig

newtype ClientUserAgent = ClientUserAgent Text
    deriving (Generic, Show)
instance FromJSON ClientUserAgent

newtype ClientMaxResponseSizeBytes = ClientMaxResponseSizeBytes Int
    deriving (Generic, Show)
instance FromJSON ClientMaxResponseSizeBytes

newtype ClientMaxCachedConnectionsPerHost = ClientMaxCachedConnectionsPerHost Int
    deriving (Generic, Show)
instance FromJSON ClientMaxCachedConnectionsPerHost

newtype ClientMaxIdleConnections = ClientMaxIdleConnections Int
    deriving (Generic, Show)
instance FromJSON ClientMaxIdleConnections

newtype ClientMaxResponseTimeout = ClientMaxResponseTimeout Int
    deriving (Generic, Show)
instance FromJSON ClientMaxResponseTimeout

-- github

data GitHubConfig = GitHubConfig
    { appId :: GitHubAppId
    , appInstallId :: GitHubAppInstallId
    , accountName :: GitHubAccountName
    , repoName :: GitHubRepoName
    , keyPath :: GitHubKeyPath
    , jwtDurationSecs :: GitHubJWTDurationSecs
    , tokenMinRemainingSecs :: GitHubTokenMinRemainingSecs
    , customPort :: GitHubCustomPort
    , urlAuth :: GitHubUrlAuth
    , urlCreatePullRequest :: GitHubUrlCreatePullRequest
    , urlCreateCheck :: GitHubUrlCreateCheck
    , urlAddComment :: GitHubUrlAddComment
    } deriving (Generic, Show)
instance FromJSON GitHubConfig

newtype GitHubAppId = GitHubAppId Text
    deriving (Generic, Show)
instance FromJSON GitHubAppId

newtype GitHubAppInstallId = GitHubAppInstallId Text
    deriving (Generic, Show)
instance FromJSON GitHubAppInstallId

newtype GitHubAccountName = GitHubAccountName Text
    deriving (Generic, Show)
instance FromJSON GitHubAccountName

newtype GitHubRepoName = GitHubRepoName Text
    deriving (Generic, Show)
instance FromJSON GitHubRepoName

newtype GitHubKeyPath = GitHubKeyPath Text
    deriving (Generic, Show)
instance FromJSON GitHubKeyPath

newtype GitHubJWTDurationSecs = GitHubJWTDurationSecs Int
    deriving (Generic, Show)
instance FromJSON GitHubJWTDurationSecs

newtype GitHubTokenMinRemainingSecs = GitHubTokenMinRemainingSecs Int
    deriving (Generic, Show)
instance FromJSON GitHubTokenMinRemainingSecs

newtype GitHubCustomPort = GitHubCustomPort Text
    deriving (Generic, Show)
instance FromJSON GitHubCustomPort

newtype GitHubUrlAuth = GitHubUrlAuth Text
    deriving (Generic, Show)
instance FromJSON GitHubUrlAuth

newtype GitHubUrlCreatePullRequest = GitHubUrlCreatePullRequest Text
    deriving (Generic, Show)
instance FromJSON GitHubUrlCreatePullRequest

newtype GitHubUrlCreateCheck = GitHubUrlCreateCheck Text
    deriving (Generic, Show)
instance FromJSON GitHubUrlCreateCheck

newtype GitHubUrlAddComment = GitHubUrlAddComment Text
    deriving (Generic, Show)
instance FromJSON GitHubUrlAddComment
