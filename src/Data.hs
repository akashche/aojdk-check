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
    ( AppState(..)
    , GitHubTokenHolder(..)
    , createGitHubTokenHolder
    -- types
    , GitHubToken(..)
    , GitHubTokenBody(..)
    , GitHubTokenExpiry(..)
    , JSONWebToken(..)
    -- getters re-export
    , TextGetter(..)
    , IntGetter(..)
    , TimeGetter(..)
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.IORef as IORef
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Time.Format as TimeFormat

import Config
import Getters

-- app state

data AppState = AppState
    { config :: Config
    , manager :: Manager
    , githubToken :: GitHubTokenHolder
    }

-- types

data GitHubToken = GitHubToken
    { token :: GitHubTokenBody
    , expires_at :: GitHubTokenExpiry
    } deriving (Generic, Show)
instance FromJSON GitHubToken
instance ToJSON GitHubToken

newtype GitHubTokenBody = GitHubTokenBody Text
    deriving (Generic, Show)
instance FromJSON GitHubTokenBody
instance ToJSON GitHubTokenBody
instance TextGetter GitHubTokenBody where
    getText (GitHubTokenBody val) = val

newtype GitHubTokenExpiry = GitHubTokenExpiry Text
    deriving (Generic, Show)
instance FromJSON GitHubTokenExpiry
instance ToJSON GitHubTokenExpiry
instance TimeGetter GitHubTokenExpiry where
    getTime (GitHubTokenExpiry val) =
        case TimeFormat.parseTimeM False TimeFormat.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (unpack val) :: Maybe UTCTime of
            Just tm -> tm
            Nothing -> error . unpack $
                "Error parsing token, date: [" <> val <> "]"

data GitHubTokenHolder = GitHubTokenHolder
    { tokenRef :: IORef.IORef GitHubToken
    , lock :: MVar.MVar Bool
    }

createGitHubTokenHolder :: IO GitHubTokenHolder
createGitHubTokenHolder = do
    let tok = GitHubToken (GitHubTokenBody "") (GitHubTokenExpiry "1970-01-01T00:00:00Z")
    ref <- IORef.newIORef tok
    mv <- MVar.newMVar True
    return $ GitHubTokenHolder ref mv

newtype JSONWebToken = JSONWebToken ByteString
    deriving Show
instance TextGetter JSONWebToken where
    getBS (JSONWebToken val) = val

