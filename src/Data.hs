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

module Data
    ( TextGetter(..)
    , IntGetter(..)
    , TimeGetter(..)
    , ServerTcpPort(..)
    , MaxResponseSizeBytes(..)
    , UserAgent(..)
    , GitHubAppId(..)
    , GitHubAppInstallId(..)
    , GitHubKeyPath(..)
    , JWTDurationSecs(..)
    , GitHubToken(..)
    , JSONWebToken(..)
    ) where

import Prelude ()
import VtUtils.Prelude

-- getters

class TextGetter a where
    getText :: a -> Text
    getText = decodeUtf8 . getBS
    getBS :: a -> ByteString
    getBS = encodeUtf8 . getText

class IntGetter a where
    getInt :: a -> Int

class TimeGetter a where
    getTime :: a -> UTCTime

-- types

newtype ServerTcpPort = ServerTcpPort Int
    deriving (Generic, Show)
instance FromJSON ServerTcpPort
instance IntGetter ServerTcpPort
    where getInt (ServerTcpPort val) = val

newtype MaxResponseSizeBytes = MaxResponseSizeBytes Int
    deriving (Generic, Show)
instance FromJSON MaxResponseSizeBytes
instance IntGetter MaxResponseSizeBytes
    where getInt (MaxResponseSizeBytes val) = val

newtype UserAgent = UserAgent Text
    deriving (Generic, Show)
instance FromJSON UserAgent
instance TextGetter UserAgent
    where getText (UserAgent val) = val

newtype GitHubAppId = GitHubAppId Text
    deriving (Generic, Show)
instance FromJSON GitHubAppId
instance TextGetter GitHubAppId
    where getText (GitHubAppId val) = val

newtype GitHubAppInstallId = GitHubAppInstallId Text
    deriving (Generic, Show)
instance FromJSON GitHubAppInstallId
instance TextGetter GitHubAppInstallId
    where getText (GitHubAppInstallId val) = val

newtype GitHubKeyPath = GitHubKeyPath Text
    deriving (Generic, Show)
instance FromJSON GitHubKeyPath
instance TextGetter GitHubKeyPath
    where getText (GitHubKeyPath val) = val

newtype JWTDurationSecs = JWTDurationSecs Int
    deriving (Generic, Show)
instance FromJSON JWTDurationSecs
instance IntGetter JWTDurationSecs
    where getInt (JWTDurationSecs val) = val

data GitHubToken = GitHubToken
    { token :: GitHubTokenBody
    , expires_at :: GitHubTokenExpiry
    } deriving (Generic, Show)
instance FromJSON GitHubToken

newtype GitHubTokenBody = GitHubTokenBody Text
    deriving (Generic, Show)
instance FromJSON GitHubTokenBody
instance TextGetter GitHubTokenBody
    where getText (GitHubTokenBody val) = val

newtype GitHubTokenExpiry = GitHubTokenExpiry Text
    deriving (Generic, Show)
instance FromJSON GitHubTokenExpiry

newtype JSONWebToken = JSONWebToken ByteString
    deriving (Show)
instance TextGetter JSONWebToken
    where getBS (JSONWebToken val) = val
