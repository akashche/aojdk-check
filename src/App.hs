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

module App
    ( App(..)
    , Config(..)
    -- server
    , ServerConfig(..)
    -- client
    , ClientConfig(..)
    , GitHubConfig(..)
    , GitHubUser(..)
    , TokenFilePath(..)
    , MaxResponseSizeBytes(..)
    -- other
    , GitHubToken(..)
    ) where

import Prelude ()
import VtUtils.Prelude

data App = App
    { config :: Config
    , manager :: Manager
    , githubToken :: GitHubToken
    }

data Config = Config
    { server :: ServerConfig
    , client :: ClientConfig
    } deriving (Generic, Show)
instance FromJSON Config

-- server config

data ServerConfig = ServerConfig
    { tcpPort :: Int
    } deriving (Generic, Show)
instance FromJSON ServerConfig

-- client config

data ClientConfig = ClientConfig
    { github :: GitHubConfig
    , maxResponseSizeBytes :: MaxResponseSizeBytes
    } deriving (Generic, Show)
instance FromJSON ClientConfig

data GitHubConfig = GitHubConfig
    { user :: GitHubUser
    , tokenFilePath :: TokenFilePath
    } deriving (Generic, Show)
instance FromJSON GitHubConfig

newtype GitHubUser = GitHubUser
    { get :: Text
    } deriving (Generic, Show)
instance FromJSON GitHubUser
    where parseJSON = genericParseJSON jsonUnwrapUnaryOptions

newtype MaxResponseSizeBytes = MaxResponseSizeBytes
    { get :: Int
    } deriving (Generic, Show)
instance FromJSON MaxResponseSizeBytes
    where parseJSON = genericParseJSON jsonUnwrapUnaryOptions

newtype TokenFilePath = TokenFilePath
    { get :: Text
    } deriving (Generic, Show)
instance FromJSON TokenFilePath
    where parseJSON = genericParseJSON jsonUnwrapUnaryOptions


-- token

newtype GitHubToken = GitHubToken
    { get ::Text
    } deriving Show




