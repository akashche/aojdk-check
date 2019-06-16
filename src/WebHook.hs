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

module WebHook
    ( webHookReceive
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Client
import Config
import Data

data User = User
    { login :: Text
    } deriving Generic
instance FromJSON User
instance ToJSON User

data Issue = Issue
    { number :: Int
--     , title :: Text
    , body :: Text
--     , user :: User
    } deriving Generic
instance FromJSON Issue
instance ToJSON Issue

data Comment = Comment
     { user :: User
--      , body :: Text
     } deriving Generic
instance FromJSON Comment
instance ToJSON Comment

data IssueOpenedHook = IssueOpenedHook
    { action :: Text
    , issue :: Issue
    } deriving Generic
instance FromJSON IssueOpenedHook
instance ToJSON IssueOpenedHook

data CommentAddedHook = CommentAddedHook
    { action :: Text
    , issue :: Issue
    , comment :: Comment
    } deriving Generic
instance FromJSON CommentAddedHook
instance ToJSON CommentAddedHook

handlers :: Vector (AppState -> Value -> IO Bool)
handlers = fromList
    [ handleCommentAdded
    , handleIssueOpened
    ]

webHookReceive :: AppState -> Value -> IO ()
webHookReceive app val = do
    let AppState {config} = app
    let Config {server} = config
    let ServerConfig {webHookActionsEnabled} = server
    let (ServerWebHookActionsEnabled enabled) = webHookActionsEnabled
    when enabled $ do
        putStrLn $ jsonEncodeText val
        handleHook app val 0
    return ()

handleHook :: AppState -> Value -> Int -> IO ()
handleHook app val idx = do
    when (idx < (Vector.length handlers)) $ do
        handled <- (handlers ! idx) app val
        when (not handled) $
            handleHook app val (idx + 1)
    return ()

handleIssueOpened :: AppState -> Value -> IO Bool
handleIssueOpened app val =
    case parseEither parseJSON val :: Either String IssueOpenedHook of
        Left _ -> return False
        Right hook@IssueOpenedHook {action, issue} -> do
            let Issue {body, number} = issue
            let num = GitHubIssueNumber number
            let cmt = GitHubIssueComment "test: issue registered with aojdk-check"
            if "opened" == action && Text.isPrefixOf "test" body then
                clientAddComment app num cmt
            else
                putStrLn $ jsonEncodeText hook
            return True

handleCommentAdded :: AppState -> Value -> IO Bool
handleCommentAdded app val =
    case parseEither parseJSON val :: Either String CommentAddedHook of
        Left _ -> return False
        Right hook@CommentAddedHook {action, issue, comment} -> do
            let Issue {body = ibody, number} = issue
            let Comment {user} = comment
            let User {login} = user
            let num = GitHubIssueNumber number
            let cmt = GitHubIssueComment "test: comment registered with aojdk-check"
            if "created" == action &&
                    Text.isPrefixOf "test" ibody &&
                    "aojdk-check-test[bot]" /= login then
                clientAddComment app num cmt
            else
                putStrLn $ jsonEncodeText hook
            return True
