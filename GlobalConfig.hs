{-# LANGUAGE TemplateHaskell #-}

module GlobalConfig
  ( GlobalConfig
  , defaultGlobalConfig
  , loadGlobalConfig
  , loadGlobalConfigFromFile
  , configBackupList
  ) where

import Snapshot (Snapshotable)

import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Yaml (ParseException, decodeEither', decodeFileEither)

data GlobalConfig =
  GlobalConfig
  { configBackupList :: [Snapshotable]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''GlobalConfig)

defaultGlobalConfig =
  GlobalConfig
  { configBackupList = []
  }

loadGlobalConfig :: ByteString -> Either ParseException GlobalConfig
loadGlobalConfig = decodeEither'

loadGlobalConfigFromFile :: FilePath -> IO (Either ParseException GlobalConfig)
loadGlobalConfigFromFile = decodeFileEither
