{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeOperators  #-}
module Polysemy.FSKVStore (
  FSKVStore
, runFSKVStoreRelBS
, runFSKVStoreAbsBS
, runFSKVStoreRelUtf8
, runFSKVStoreAbsUtf8
) where

import           Data.ByteString         as BS
import           Path
import           Polysemy
import           Polysemy.KVStore
import           RIO                     (Text, readFileUtf8, writeFileUtf8)
import qualified UnliftIO.Path.Directory as U

-- | Type synonym for a KVStore indexed by files.
type FSKVStore b a = KVStore (Path b File) a

-- | Run an `FSKVStore Rel ByteString` in the supplied directory in IO.
runFSKVStoreRelBS :: Members '[Embed IO] r
                  => Path b Dir
                  -> Sem (FSKVStore Rel ByteString ': r) a
                  -> Sem r a
runFSKVStoreRelBS d = interpret \case
  LookupKV k   -> embed $ do
    z <- U.doesFileExist (d </> k)
    if z
      then fmap Just . BS.readFile $ toFilePath $ d </> k
      else return Nothing
  UpdateKV k v -> embed $ do
    U.createDirectoryIfMissing True (d </> parent k)
    case v of
      Nothing -> pure ()
      Just x  -> BS.writeFile (toFilePath (d </> k)) x

-- | Run an `FSKVStore Abs ByteString` in IO.
runFSKVStoreAbsBS :: Members '[Embed IO] r
                  => Sem (FSKVStore Abs ByteString ': r) a
                  -> Sem r a
runFSKVStoreAbsBS = interpret \case
  LookupKV k   -> embed $ do
    z <- U.doesFileExist k
    if z
      then fmap Just . BS.readFile $ toFilePath k
      else return Nothing
  UpdateKV k v -> embed $ do
    U.createDirectoryIfMissing True (parent k)
    case v of
      Nothing -> pure ()
      Just x  -> BS.writeFile (toFilePath k) x

-- | Run an `FSKVStore Rel Text` in the supplied directory in IO as UTF8.
runFSKVStoreRelUtf8 :: Members '[Embed IO] r
                    => Path b Dir
                    -> Sem (FSKVStore Rel Text ': r) a
                    -> Sem r a
runFSKVStoreRelUtf8 d = interpret \case
  LookupKV k   -> do
    z <- U.doesFileExist (d </> k)
    if z
      then fmap Just . readFileUtf8 $ toFilePath $ d </> k
      else return Nothing
  UpdateKV k v -> do
    U.createDirectoryIfMissing True (d </> parent k)
    case v of
      Nothing -> pure ()
      Just x  -> writeFileUtf8 (toFilePath (d </> k)) x

-- | Run an `FSKVStore Abs Text` in IO as UTF8.
runFSKVStoreAbsUtf8 :: Members '[Embed IO] r
                    => Sem (FSKVStore Abs Text ': r) a
                    -> Sem r a
runFSKVStoreAbsUtf8 = interpret \case
  LookupKV k   -> do
    z <- U.doesFileExist k
    if z
      then fmap Just . readFileUtf8 $ toFilePath k
      else return Nothing
  UpdateKV k v -> do
    U.createDirectoryIfMissing True (parent k)
    case v of
      Nothing -> pure ()
      Just x  -> writeFileUtf8 (toFilePath k) x
