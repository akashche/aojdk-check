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
    bs <- digestSignRS256 "test/data/test.private-key.pem" "foo2"
    assertEqual "sign" "AijZaMANraPlr5cetcl8pzpoK6wuWJ2pgOdThrEtl9T5AdlXsJRMx2XMIKDLhOftq4pndc4N6VQTonX6J6zEg8XnvIZkuQB_mUo7zCS9C_fOSzpK5Ay5IFCltbsg72GTQvr7Eg3lZRSMqaOfKNzkVBl2WvcUQwiVsWbrQ6uX9cAOXvioh5URu35ER4jYtX8oqtxutvE4U9_rbWTvC1PkHQAySqkRX3HUtqiS9nxIqtGUHTm7tDZ48OcHg-io01D1e9bP9oFCO5zeEaJgCoHcFkOVDx9cLHCnZL2cDf-uiWADxGcrl5Dz4hPjqtJmg79NUf6ngv2RfDDDwhsiqP4WRg==" $
        (decodeUtf8 . Base64URL.encode) bs
    return ()

digestTest :: Test
digestTest = TestLabel "DigestTest" (TestList
    [ testSign
    ])
