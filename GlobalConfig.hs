{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module GlobalConfig
  ( GlobalConfig
  , defaultGlobalConfig
  , loadGlobalConfig
  , loadGlobalConfigFromFile
  , configBackupList
  ) where

import Snapshot (Snapshotable)

import Data.Aeson (FromJSON, (.:?), (.!=), parseJSON, withObject)
import Data.Aeson.TH (deriveToJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Yaml (ParseException, decodeEither', decodeFileEither)

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

loadGlobalConfigFromFile :: FilePath -> IO (Either ParseException GlobalConfig)
loadGlobalConfigFromFile = decodeFileEither
