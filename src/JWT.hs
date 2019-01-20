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

module JWT
    ( jwtCreate
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Lazy as ByteStringLazy

import Digest

base64Json :: Value -> ByteString
base64Json =
    Base64URL.encode . ByteStringLazy.toStrict . encodePretty

jwtCreate :: Text -> Text -> Int -> IO ByteString
jwtCreate key iss dur = do
    now <- (floor . utcTimeToPOSIXSeconds) <$> getCurrentTime
    let header = object
            [ "alg" .= ("RS256" :: Text)
            , "typ" .= ("JWT" :: Text)
            ]
    let body = object
            [ "iat" .= now
            , "exp" .= (now + dur)
            , "iss" .= iss
            ]
    let bs = (base64Json header) <> "." <> (base64Json body)
    sign <- Base64URL.encode <$> digestSignRS256 key bs
    return $ ByteString.concat [bs, ".", sign]
