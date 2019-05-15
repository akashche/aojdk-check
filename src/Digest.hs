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

module Digest
    ( digestSignRS256
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.Vector.Storable.Mutable as Mutable

foreign import ccall "EVP_MD_CTX_create" c'EVP_MD_CTX_create
    :: IO (Ptr ())

foreign import ccall "EVP_MD_CTX_destroy" c'EVP_MD_CTX_destroy
    :: Ptr () -> IO ()

foreign import ccall "EVP_get_digestbyname" c'EVP_get_digestbyname
    :: CString -> IO (Ptr ())

foreign import ccall "EVP_DigestInit_ex" c'EVP_DigestInit_ex
    :: Ptr () -> Ptr () -> Ptr () -> IO CInt

foreign import ccall "BIO_s_file" c'BIO_s_file
    :: IO (Ptr ())

foreign import ccall "BIO_new" c'BIO_new
    :: Ptr () -> IO (Ptr ())

foreign import ccall "BIO_free_all" c'BIO_free_all
    :: Ptr () -> IO ()

foreign import ccall "BIO_ctrl" c'BIO_ctrl
    :: Ptr () -> CInt -> CLong -> CString -> IO CLong

foreign import ccall "PEM_read_bio_PrivateKey" c'PEM_read_bio_PrivateKey
    :: Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())

foreign import ccall "EVP_PKEY_free" c'EVP_PKEY_free
    :: Ptr () -> IO ()

foreign import ccall "EVP_DigestSignInit" c'EVP_DigestSignInit
    :: Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr() -> IO CInt

foreign import ccall "EVP_DigestUpdate" c'EVP_DigestUpdate
    :: Ptr () -> CString -> CSize -> IO CInt

foreign import ccall "EVP_DigestSignFinal" c'EVP_DigestSignFinal
    :: Ptr () -> Ptr Word8 -> Ptr() -> IO CInt


withEVP_MD_CTX :: (Ptr () -> IO a) -> IO a
withEVP_MD_CTX fun =
    bracket
        ( do
            ctx <- c'EVP_MD_CTX_create
            when (nullPtr == ctx) $ error .unpack $
                   "FFI call error,"
                <> " call: [EVP_MD_CTX_create]"
            return ctx )
        (\ctx -> c'EVP_MD_CTX_destroy ctx)
        fun

withBIO_s_file :: (Ptr() -> IO a) -> IO a
withBIO_s_file fun =
    bracket
        ( do
            tp <- c'BIO_s_file
            bio <- c'BIO_new tp
            when (nullPtr == bio) $ error . unpack $
                   "FFI call error,"
                <> " call: [BIO_new]"
            return bio )
        (\bio -> c'BIO_free_all bio)
        fun

withPrivateKey :: Text -> Ptr () -> (Ptr () -> IO a) -> IO a
withPrivateKey label bio fun =
    bracket
        ( do
            key <- c'PEM_read_bio_PrivateKey bio nullPtr nullPtr nullPtr
            when (nullPtr == key) $ error . unpack $
                   "FFI call error,"
                <> " call: [PEM_read_bio_PrivateKey],"
                <> " path: [" <> label <> "]"
            return key )
        (\key -> c'EVP_PKEY_free key)
        fun

getDigestByName :: Text -> IO (Ptr ())
getDigestByName name = do
    ffiWithUTF8 name $ \cs -> do
        md <- c'EVP_get_digestbyname cs
        when (nullPtr == md) $ error .unpack $
               "FFI call error,"
            <> " call: [EVP_get_digestbyname],"
            <> " name: [" <> name <> "]"
        return md

initDigestCtx :: Ptr () -> Text -> Text -> IO ()
initDigestCtx ctx hash key = do
    md <- getDigestByName hash
    errInit <- c'EVP_DigestInit_ex ctx md nullPtr
    when (1 /= errInit) $ error . unpack $
               "FFI call error,"
            <> " call: [EVP_DigestInit_ex]"
    withBIO_s_file $ \bio -> do
        ffiWithUTF8 key $ \nm -> do
            -- BIO_read_filename
            errRead <- c'BIO_ctrl bio 108 (0x01 .|. 0x02) nm
            when (1 /= errRead) $ error . unpack $
                   "FFI call error,"
                <> " call: [BIO_read_filename],"
                <> " path: [" <> key <> "]"
            withPrivateKey key bio $ \pkey -> do
                errInit2 <- c'EVP_DigestSignInit ctx nullPtr md nullPtr pkey
                when (1 /= errInit2) $ error . unpack $
                       "FFI call error,"
                    <> " call: [EVP_DigestSignInit],"
                    <> " path: [" <> key <> "]"

digestSignRS256 :: Text -> ByteString -> IO ByteString
digestSignRS256 key text =
    withEVP_MD_CTX $ \ctx -> do
        -- initialize digest
        initDigestCtx ctx "SHA256" key
        -- update digest
        useAsCStringLen text $ \(dt, dlen) -> do
            err <- c'EVP_DigestUpdate ctx dt (fromIntegral dlen)
            when (1 /= err) $ error . unpack $
                   "FFI call error,"
                <> " call: [EVP_DigestUpdate],"
                <> " data len: [" <> (textShow dlen) <> "]"
        -- finalize digest
        len <- ffiWithPtr (0 :: CSize) $ \req -> do
            err <- c'EVP_DigestSignFinal ctx nullPtr (castPtr req)
            when (1 /= err) $ error . unpack $
                   "FFI call error,"
                <> " call: [EVP_DigestSignFinal]"
            len <- peek req
            return len
        -- read signature
        vec <- Mutable.replicate (fromIntegral len) (0 :: Word8)
        Mutable.unsafeWith vec $ \buf -> do
            ffiWithPtr len $ \lptr -> do
                err <- c'EVP_DigestSignFinal ctx buf (castPtr lptr)
                when (1 /= err) $ error . unpack $
                       "FFI call error,"
                    <> " call: [EVP_DigestSignFinal],"
                    <> " len: [" <> (textShow len) <> "]"
                bs <- packCStringLen ((castPtr buf), (fromIntegral len))
                return bs
