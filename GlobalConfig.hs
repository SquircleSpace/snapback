{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module GlobalConfig
  ( GlobalConfig
  , HasGlobalConfig
  , getGlobalConfig
  , defaultGlobalConfig
  , loadGlobalConfig
  , loadGlobalConfigFromFile
  , configBackupList
  , backupList
  , globalConfig
  ) where

import Snapshot (Snapshotable)

import Control.Monad.Except (MonadError, throwError, runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (FromJSON, (.:?), (.!=), parseJSON, withObject)
import Data.Aeson.TH (deriveToJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Yaml (ParseException, decodeEither', decodeFileEither)

class HasGlobalConfig val where
  getGlobalConfig :: val -> GlobalConfig

instance HasGlobalConfig GlobalConfig where
  getGlobalConfig = id

backupList :: (MonadReader env m, HasGlobalConfig env) => m [Snapshotable]
backupList = asks (configBackupList . getGlobalConfig)

globalConfig :: (MonadReader env m, HasGlobalConfig env) => m GlobalConfig
globalConfig = asks getGlobalConfig

data GlobalConfig =
  GlobalConfig
  { configBackupList :: [Snapshotable]
  } deriving (Eq, Show)

$(deriveToJSON defaultOptions ''GlobalConfig)

defaultGlobalConfig :: GlobalConfig
defaultGlobalConfig =
  GlobalConfig
  { configBackupList = []
  }

defaultFor :: (GlobalConfig -> a) -> a
defaultFor fn = fn defaultGlobalConfig

instance FromJSON GlobalConfig where
  parseJSON = withObject "GlobalConfig" $ \object ->
    GlobalConfig <$> object .:? "configBackupList" .!= defaultFor configBackupList

loadGlobalConfig :: ByteString -> Either ParseException GlobalConfig
loadGlobalConfig = decodeEither'

loadGlobalConfigFromFileEither :: MonadIO m => FilePath -> m (Either ParseException GlobalConfig)
loadGlobalConfigFromFileEither path = liftIO $ decodeFileEither path

loadGlobalConfigFromFile :: (MonadIO m, MonadError ParseException m) => FilePath -> m GlobalConfig
loadGlobalConfigFromFile path = liftIO (decodeFileEither path) >>= liftEither
