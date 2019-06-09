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

module Git
    ( GitRepo
    , GitFFICallException(..)
    , gitWithRepo
    , gitCreateBranch
    , gitCommitBranch
    , gitPushBranch
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.Vector.Storable as VectorStorable

-- types for opaque pointers

newtype C'git_error = C'git_error ()
newtype C'git_repository = C'git_repository ()
newtype C'git_reference = C'git_reference ()
newtype C'git_commit = C'git_commit ()
newtype C'git_index = C'git_index ()
newtype C'git_signature = C'git_signature ()
newtype C'git_tree = C'git_tree ()
newtype C'git_remote = C'git_remote ()
_suppress :: IO ()
_suppress = do
    let _ = C'git_error
    let _ = C'git_repository
    let _ = C'git_reference
    let _ = C'git_commit
    let _ = C'git_index
    let _ = C'git_signature
    let _ = C'git_tree
    let _ = C'git_remote
    return ()

-- non-opaque structs

data GitFFIOidPeekNotSupportedException = GitFFIOidPeekNotSupportedException
instance Exception GitFFIOidPeekNotSupportedException
instance Show GitFFIOidPeekNotSupportedException where
    show e = errorShow e $
              "Peek operation not supported"

data GitFFIOidInvalidLengthException = GitFFIOidInvalidLengthException
    { oidLength :: Int
    }
instance Exception GitFFIOidInvalidLengthException
instance Show GitFFIOidInvalidLengthException where
    show e@(GitFFIOidInvalidLengthException {oidLength}) = errorShow e $
               "Invalid OID specified,"
            <> " length: [" <> (textShow oidLength) <> "]"

-- https://libgit2.org/libgit2/#v0.24.1/type/git_oid
data C'git_oid = C'git_oid (VectorStorable.Vector CUChar)
instance Storable C'git_oid where
    sizeOf ~_ = 20
    alignment ~_ = 1
    peek _ = throwIO $ GitFFIOidPeekNotSupportedException
    poke ptr (C'git_oid vec) = do
        let len = VectorStorable.length vec
        when (20 /= len) $ throwIO $ GitFFIOidInvalidLengthException len
        VectorStorable.unsafeWith vec $ \buf -> do
            let dest = castPtr ptr
            copyBytes dest buf 20
            return ()

data C'git_strarray = C'git_strarray (Ptr CString) CSize
instance Storable C'git_strarray where
    sizeOf ~_ = sizeOf (undefined :: Ptr CString) + sizeOf (undefined :: CSize)
    alignment ~_ = sizeOf (undefined :: Word) -- todo: checkme on 32-bit
    peek _ = throwIO $ GitFFIOidPeekNotSupportedException
    poke p (C'git_strarray v0 v1) = do
        pokeByteOff p 0 v0
        let off = sizeOf (undefined :: Ptr CString)
        pokeByteOff p off v1
        return ()

-- shortcuts

type GitRepo = Ptr C'git_repository


-- borrowed from: https://hackage.haskell.org/package/hlibgit2

foreign import ccall "git_libgit2_init" c'git_libgit2_init
    :: IO (CInt)

foreign import ccall "git_libgit2_shutdown" c'git_libgit2_shutdown
    :: IO (CInt)

foreign import ccall "giterr_last" c'giterr_last
    :: IO (Ptr C'git_error)

foreign import ccall "git_repository_open" c'git_repository_open
    :: Ptr (Ptr C'git_repository) -> CString -> IO (CInt)

foreign import ccall "git_repository_free" c'git_repository_free
    :: Ptr C'git_repository -> IO ()

foreign import ccall "git_reference_free" c'git_reference_free
    :: Ptr C'git_reference -> IO ()

foreign import ccall "git_commit_lookup" c'git_commit_lookup
    :: Ptr (Ptr C'git_commit) -> Ptr C'git_repository -> Ptr C'git_oid -> IO (CInt)

foreign import ccall "git_commit_free" c'git_commit_free
    :: Ptr C'git_commit -> IO ()

foreign import ccall "git_branch_create" c'git_branch_create
    :: Ptr (Ptr C'git_reference) -> Ptr C'git_repository -> CString -> Ptr C'git_commit -> CInt -> IO (CInt)

foreign import ccall "git_repository_index" c'git_repository_index
    :: Ptr (Ptr C'git_index) -> Ptr C'git_repository -> IO (CInt)

foreign import ccall "git_index_free" c'git_index_free
    :: Ptr C'git_index -> IO ()

foreign import ccall "git_signature_new" c'git_signature_new
    :: Ptr (Ptr C'git_signature) -> CString -> CString -> CLong -> CInt -> IO (CInt)

foreign import ccall "git_signature_free" c'git_signature_free
    :: Ptr C'git_signature -> IO ()

foreign import ccall "git_tree_lookup" c'git_tree_lookup
    :: Ptr (Ptr C'git_tree) -> Ptr C'git_repository -> Ptr C'git_oid -> IO (CInt)

foreign import ccall "git_tree_free" c'git_tree_free
    :: Ptr C'git_tree -> IO ()

foreign import ccall "git_remote_create_anonymous" c'git_remote_create_anonymous
    :: Ptr (Ptr C'git_remote) -> Ptr C'git_repository -> CString -> IO (CInt)

foreign import ccall "git_remote_free" c'git_remote_free
    :: Ptr C'git_remote -> IO ()

foreign import ccall "git_reference_name_to_id" c'git_reference_name_to_id
    :: Ptr C'git_oid -> Ptr C'git_repository -> CString -> IO (CInt)

foreign import ccall "git_repository_set_head" c'git_repository_set_head
    :: Ptr C'git_repository -> CString -> IO (CInt)

foreign import ccall "git_index_add_bypath" c'git_index_add_bypath
    :: Ptr C'git_index -> CString -> IO (CInt)

foreign import ccall "git_index_write" c'git_index_write
    :: Ptr C'git_index -> IO (CInt)

foreign import ccall "git_index_write_tree" c'git_index_write_tree
    :: Ptr C'git_oid -> Ptr C'git_index -> IO (CInt)

foreign import ccall "git_commit_create" c'git_commit_create
    :: Ptr C'git_oid -> Ptr C'git_repository -> CString -> Ptr C'git_signature -> Ptr C'git_signature -> CString -> CString -> Ptr C'git_tree -> CInt -> Ptr (Ptr C'git_commit) -> IO (CInt)

foreign import ccall "git_remote_push" c'git_remote_push
    :: Ptr C'git_remote -> Ptr C'git_strarray -> Ptr () -> IO (CInt)


-- internal functions

data GitFFICallException = GitFFICallException
    { callName :: Text
    , errorCode :: CInt
    , errorMessage :: Text
    }
instance Exception GitFFICallException
instance Show GitFFICallException where
    show e@(GitFFICallException {callName, errorCode, errorMessage}) = errorShow e $
              "FFI call error,"
            <> " call: [" <> callName <> "],"
            <> " code: [" <> (textShow errorCode) <> "],"
            <> " message: [" <> errorMessage <> "]"

checkErr :: Text -> IO CInt -> IO ()
checkErr name fun = do
    err <- fun
    when (0 /= err) $ do
        eptr <- c'giterr_last
        msg <- if nullPtr /= eptr then do
            let csptr = castPtr eptr
            cs <- peek csptr
            bs <- packCString cs
            return (textDecodeUtf8 bs)
        else
            return ""
        throwIO $ GitFFICallException name err msg

withOid :: (Ptr C'git_oid -> IO a) -> IO a
withOid fun = do
    let vec = VectorStorable.replicate 20 0
    ffiWithPtr (C'git_oid vec) fun

withCommit :: GitRepo -> Ptr C'git_oid -> (Ptr C'git_commit -> IO a) -> IO a
withCommit repo oid fun =
    ffiWithPtrPtr $ \pcmt -> bracket
        ( do
            checkErr "git_commit_lookup" $
                c'git_commit_lookup pcmt repo oid
            cmt <- peek pcmt :: IO (Ptr C'git_commit)
            return cmt )
        (\cmt -> c'git_commit_free cmt)
        fun

withNewBranch :: GitRepo -> Text -> Ptr C'git_commit -> (Ptr C'git_reference -> IO a) -> IO a
withNewBranch repo name cmt fun =
    ffiWithPtrPtr $ \pref -> bracket
        (do
            checkErr "git_branch_create" $ ffiWithUTF8 name $ \cs->
                c'git_branch_create pref repo cs cmt 0
            testb <- peek pref :: IO (Ptr C'git_reference)
            return testb )
        (\ref -> c'git_reference_free ref)
        fun

withIndex :: GitRepo -> (Ptr C'git_index -> IO a) -> IO a
withIndex repo fun =
    ffiWithPtrPtr $ \pidx -> bracket
        ( do
            checkErr "git_repository_index" $
                c'git_repository_index pidx repo
            idx <- peek pidx :: IO (Ptr C'git_index)
            return idx )
        (\idx -> c'git_index_free idx)
        fun

withSignature :: Text -> Text -> UTCTime -> (Ptr C'git_signature -> IO a) -> IO a
withSignature name email time fun =
    ffiWithPtrPtr $ \psig -> bracket
        ( do
            let secs = (floor . utcTimeToPOSIXSeconds) time
            ffiWithUTF8 name $ \cname ->
                ffiWithUTF8 email $ \cemail ->
                    checkErr "git_signature_new" $
                        c'git_signature_new psig cname cemail secs 0
            sig <- peek psig :: IO (Ptr C'git_signature)
            return sig )
        (\sig -> c'git_signature_free sig)
        fun

withTree :: GitRepo -> Ptr C'git_oid -> (Ptr C'git_tree ->IO a) -> IO a
withTree repo oid fun =
    ffiWithPtrPtr $ \ptree -> bracket
        ( do
            checkErr "git_tree_lookup" $
                c'git_tree_lookup ptree repo oid
            tree <- peek ptree :: IO (Ptr C'git_tree)
            return tree )
        (\tree -> c'git_tree_free tree)
        fun

withRemote :: GitRepo -> Text -> (Ptr C'git_remote -> IO a) -> IO a
withRemote repo url fun =
    ffiWithPtrPtr $ \premote -> bracket
        ( do
            checkErr "git_remote_create" $
                ffiWithUTF8 url $ \curl ->
                    c'git_remote_create_anonymous premote repo curl
            remote <- peek premote :: IO (Ptr C'git_remote)
            return remote )
        (\remote -> c'git_remote_free remote)
        fun

withStrArray :: Text -> (Ptr C'git_strarray -> IO a) -> IO a
withStrArray tx fun =
    ffiWithUTF8 tx $ \cs ->
        ffiWithPtr cs $ \pcs -> do
            let arr = C'git_strarray pcs 1
            ffiWithPtr arr fun


-- exported functions

gitWithRepo :: Text -> (GitRepo -> IO a) -> IO a
gitWithRepo path fun =
    ffiWithPtrPtr $ \prepo -> bracket
        ( do
            _ <- c'git_libgit2_init
            checkErr "git_repository_open" $
                ffiWithUTF8 path $ \cs ->
                    c'git_repository_open prepo cs
            repo <- peek prepo :: IO GitRepo
            return repo )
        (\repo -> do
            c'git_repository_free repo
            c'git_libgit2_shutdown )
        fun

gitCreateBranch :: GitRepo -> Text -> Text -> IO ()
gitCreateBranch repo ref branch =
    withOid $ \oid -> do
        -- lookup src ref
        checkErr "git_reference_name_to_id" $
            ffiWithUTF8 ref $ \cs ->
                c'git_reference_name_to_id oid repo cs
        -- lookup commit
        withCommit repo oid $ \cmt -> do
            -- create branch
            withNewBranch repo branch cmt $ \_ ->
                return ()
        -- checkout branch
        checkErr "git_repository_set_head" $
            ffiWithUTF8 ("refs/heads/" <> branch) $ \cs ->
                c'git_repository_set_head repo cs

gitCommitBranch :: GitRepo -> Text -> Vector Text -> Text -> Text -> Text -> IO ()
gitCommitBranch repo branch paths username email message =
    withOid $ \treeoid -> do
        dt <- getCurrentTime
        -- stage
        withIndex repo $ \idx -> do
            -- stage files
            forM_ paths $ \pa ->
                checkErr "git_index_add_bypath" $
                    ffiWithUTF8 pa $ \cs ->
                        c'git_index_add_bypath idx cs
            -- write index to disk
            checkErr "git_index_write" $
                c'git_index_write idx
            -- write index tree, fill oid
            checkErr "git_index_write_tree" $
                c'git_index_write_tree treeoid idx
        -- commit
        withOid $ \parentoid -> do
            -- lookup HEAD for parent commit
            checkErr "git_reference_name_to_id" $
                ffiWithUTF8 ("refs/heads/" <> branch) $ \cs ->
                    c'git_reference_name_to_id parentoid repo cs
            -- create commit
            checkErr "git_commit_create" $
                withOid $ \oid -> -- output commit id
                ffiWithUTF8 ("refs/heads/" <> branch) $ \uref -> -- branch to update
                withSignature username email dt $ \sig -> -- author and committer
                ffiWithUTF8 "UTF8" $ \utf8 -> -- message encoding
                ffiWithUTF8 message $ \msg -> -- message
                withTree repo treeoid $ \tree -> -- tree to commit
                withCommit repo parentoid $ \parentcmt -> -- parent commit
                    ffiWithPtr parentcmt $ \pcmt -> -- parent commit ptr
                        c'git_commit_create oid repo uref sig sig utf8 msg tree 1 pcmt

gitPushBranch ::  GitRepo -> Text -> Text -> IO ()
gitPushBranch repo branch url =
    withRemote repo url $ \remote -> do
        let spec = "refs/heads/" <> branch <>":refs/heads/" <> branch
        withStrArray spec $ \sa ->
            checkErr "git_remote_push" $
                c'git_remote_push remote sa nullPtr
