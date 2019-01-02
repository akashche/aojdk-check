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

module Main where

import Prelude ()
import VtUtils.Prelude

import Config
import Server

main :: IO ()
main = do
    args <- (fmap pack) <$> fromList <$> getArgs
    if 1 /= length args then do
        putStrLn $ "Error: Invalid arguments specified, args: [" <> textShow args <> "]"
        putStrLn $ "Usage: [aojdk-check-exe <path/to/config.json>]"
    else do
        let cfpath = args ! 0
        cf <- jsonDecodeFile cfpath :: IO Config
        putStrLn $ "Starting server ..."
        serverRun cf
