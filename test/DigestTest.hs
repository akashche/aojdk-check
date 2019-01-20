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

module DigestTest (digestTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude
import qualified Data.ByteString.Base64.URL as Base64URL

import Digest

testSign :: Test
testSign = TestLabel "testSign" $ TestCase $ do
    bs <- digestSignRS256 ".secret/aojdk-check-test.2019-01-20.private-key.pem" "foo2"
    assertEqual "sign" "o0ZGF2sZJK_LU7l_tIPfE0IkheYJKi7uSdROEdfZQ-6f_jC1DYirksZsMLVdyaEgIfW-69kMUtWLIy5VkI1HGcjyagalIkXN2Ci30VF7w-UeG9QWt5bwNHEqOOupwj7gyFgEqhqLNcnToUAyN78XyyGXAeHONv2uAVaWgioPdPVt4gZstYOXa1mqod40LT8MQcORQTUg9oBy_2i-xJWRKmZvGzVPZ5bg8oNyMvlYpWu-y1ZSYb3oR42CXibPKMKrtenY-lrMDWJpi3swpgIyBsymm6KgAhMM5-A3nKcXKpecdIMyxi98huR3OpelNIcsPc3k48u3Hq7IovB9RffpaA==" $
        (decodeUtf8 . Base64URL.encode) bs
    return ()

digestTest :: Test
digestTest = TestLabel "DigestTest" (TestList
    [ testSign
    ])
