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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Config
    ( Config(..)
    -- server
    , ServerConfig(..)
    , ServerTcpPort(..)
    -- client
    , ClientConfig(..)
    , ClientUserAgent(..)
    , ClientMaxResponseSizeBytes(..)
    -- github
    , GitHubConfig(..)
    , GitHubAppId(..)
    , GitHubAppInstallId(..)
    , GitHubKeyPath(..)
    , GitHubJWTDurationSecs(..)
    , GitHubUrlAuth(..)
    ) where

import Prelude ()
import VtUtils.Prelude

import Getters

data Config = Config
    { server :: ServerConfig
    , client :: ClientConfig
    , github :: GitHubConfig
    } deriving (Generic, Show)
instance FromJSON Config

-- server

data ServerConfig = ServerConfig
    { tcpPort :: ServerTcpPort
    } deriving (Generic, Show)
instance FromJSON ServerConfig

newtype ServerTcpPort = ServerTcpPort Int
    deriving (Generic, Show)
instance FromJSON ServerTcpPort
instance IntGetter ServerTcpPort where
    getInt (ServerTcpPort val) = val

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
instance TextGetter ClientUserAgent where
    getText (ClientUserAgent val) = val

newtype ClientMaxResponseSizeBytes = ClientMaxResponseSizeBytes Int
    deriving (Generic, Show)
instance FromJSON ClientMaxResponseSizeBytes
instance IntGetter ClientMaxResponseSizeBytes where
    getInt (ClientMaxResponseSizeBytes val) = val

newtype ClientMaxCachedConnectionsPerHost = ClientMaxCachedConnectionsPerHost Int
    deriving (Generic, Show)
instance FromJSON ClientMaxCachedConnectionsPerHost
instance IntGetter ClientMaxCachedConnectionsPerHost where
    getInt (ClientMaxCachedConnectionsPerHost val) = val

newtype ClientMaxIdleConnections = ClientMaxIdleConnections Int
    deriving (Generic, Show)
instance FromJSON ClientMaxIdleConnections
instance IntGetter ClientMaxIdleConnections where
    getInt (ClientMaxIdleConnections val) = val

newtype ClientMaxResponseTimeout = ClientMaxResponseTimeout Int
    deriving (Generic, Show)
instance FromJSON ClientMaxResponseTimeout
instance IntGetter ClientMaxResponseTimeout where
    getInt (ClientMaxResponseTimeout val) = val

-- github

data GitHubConfig = GitHubConfig
    { appId :: GitHubAppId
    , appInstallId :: GitHubAppInstallId
    , keyPath :: GitHubKeyPath
    , jwtDurationSecs :: GitHubJWTDurationSecs
    , urlAuth :: GitHubUrlAuth
    , tokenMinRemainingSecs :: GitHubTokenMinRemainingSecs
    } deriving (Generic, Show)
instance FromJSON GitHubConfig

newtype GitHubAppId = GitHubAppId Text
    deriving (Generic, Show)
instance FromJSON GitHubAppId
instance TextGetter GitHubAppId where
    getText (GitHubAppId val) = val

newtype GitHubAppInstallId = GitHubAppInstallId Text
    deriving (Generic, Show)
instance FromJSON GitHubAppInstallId
instance TextGetter GitHubAppInstallId where
    getText (GitHubAppInstallId val) = val

newtype GitHubKeyPath = GitHubKeyPath Text
    deriving (Generic, Show)
instance FromJSON GitHubKeyPath
instance TextGetter GitHubKeyPath where
    getText (GitHubKeyPath val) = val

newtype GitHubJWTDurationSecs = GitHubJWTDurationSecs Int
    deriving (Generic, Show)
instance FromJSON GitHubJWTDurationSecs
instance IntGetter GitHubJWTDurationSecs where
    getInt (GitHubJWTDurationSecs val) = val

newtype GitHubUrlAuth = GitHubUrlAuth Text
    deriving (Generic, Show)
instance FromJSON GitHubUrlAuth
instance TextGetter GitHubUrlAuth where
    getText (GitHubUrlAuth val) = val

newtype GitHubTokenMinRemainingSecs = GitHubTokenMinRemainingSecs Int
    deriving (Generic, Show)
instance FromJSON GitHubTokenMinRemainingSecs
instance IntGetter GitHubTokenMinRemainingSecs where
    getInt (GitHubTokenMinRemainingSecs val) = val
