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

module Data
    ( AppState(..)
    , appStateCustomizePort
    , GitHubTokenHolder(..)
    , createGitHubTokenHolder
    -- types
    , GitHubToken(..)
    , emptyGitHubToken
    , GitHubTokenBody(..)
    , GitHubTokenExpiry(..)
    , githubTokenExpiryTime
    , JSONWebToken(..)
    , FetchURL(..)
    , GitHubURL(..)
    , GitHubRequestPR(..)
    , GitHubRequestCheck(..)
    , GitHubIssueNumber(..)
    , GitHubIssueComment(..)
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.IORef as IORef
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Time.Format as TimeFormat

import Config

-- app state

data AppState = AppState
    { config :: Config
    , manager :: Manager
    , githubToken :: GitHubTokenHolder
    }

appStateCustomizePort :: AppState -> Int -> AppState
appStateCustomizePort app port =
    let
        AppState {config} = app
        Config {github} = config
    in
        app
            { config = config
                { github = github
                    { customPort = GitHubCustomPort $ textShow port
                    }
                }
            }

-- types

data GitHubToken = GitHubToken
    { token :: GitHubTokenBody
    , expires_at :: GitHubTokenExpiry
    } deriving (Generic, Show)
instance FromJSON GitHubToken
instance ToJSON GitHubToken

emptyGitHubToken :: GitHubToken
emptyGitHubToken =
    GitHubToken (GitHubTokenBody "") (GitHubTokenExpiry "1970-01-01T00:00:00Z")

newtype GitHubTokenBody = GitHubTokenBody Text
    deriving (Generic, Show)
instance FromJSON GitHubTokenBody
instance ToJSON GitHubTokenBody

newtype GitHubTokenExpiry = GitHubTokenExpiry Text
    deriving (Generic, Show)
instance FromJSON GitHubTokenExpiry
instance ToJSON GitHubTokenExpiry

githubTokenExpiryTime :: GitHubTokenExpiry -> Maybe UTCTime
githubTokenExpiryTime (GitHubTokenExpiry val) =
        TimeFormat.parseTimeM False TimeFormat.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (unpack val)

data GitHubTokenHolder = GitHubTokenHolder
    { tokenRef :: IORef.IORef GitHubToken
    , lock :: MVar.MVar Bool
    }

createGitHubTokenHolder :: IO GitHubTokenHolder
createGitHubTokenHolder = do
    ref <- IORef.newIORef emptyGitHubToken
    mv <- MVar.newMVar True
    return $ GitHubTokenHolder ref mv

newtype JSONWebToken = JSONWebToken ByteString
    deriving Show

newtype FetchURL = FetchURL Text
    deriving Show

newtype GitHubURL = GitHubURL Text
    deriving Show

-- fields are not used separately
data GitHubRequestPR = GitHubRequestPR
    { title :: Text
    , head :: Text
    , base :: Text
    , body :: Text
    } deriving (Generic, Show)
instance ToJSON GitHubRequestPR

-- fields are not used separately
data GitHubRequestCheck = GitHubRequestCheck
    { name :: Text
    , head_sha :: Text
    , details_url :: Text
    , external_id :: Text
    } deriving (Generic, Show)
instance ToJSON GitHubRequestCheck

newtype GitHubIssueNumber = GitHubIssueNumber Int
    deriving Show

newtype GitHubIssueComment = GitHubIssueComment Text
    deriving Show
