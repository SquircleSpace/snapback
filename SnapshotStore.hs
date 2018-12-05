{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module SnapshotStore
  ( Subvol(..)
  , SnapshotStore
  , Local(..)
  , Remote(..)
  , storeSnapshots
  , storePath
  , mkSnapshotStore
  , loadSnapshotStore
  , loadSnapshotStoreWithFilter
  , findSubvolsInPath
  , lookupSubvolPathToParent
  , validateSubvol
  , compareStores
  ) where

import Control.Monad (when, liftM, forM, filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, FromJSON, Array, Value, toJSON, toEncoding, parseJSON, withArray)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (Parser)
import Data.Either (isRight)
import Data.Foldable (foldr')
import Data.List (sortBy)
import Data.Map.Strict (Map, fromList, intersectionWith, elems, difference)
import Data.Maybe (catMaybes, isJust)
import System.FilePath ((</>), splitFileName, makeRelative, isRelative)
import System.Linux.Btrfs (InodeNum, SubvolId, SubvolInfo(..), lookupInode, childSubvols, getSubvolInfo)
import System.Linux.Btrfs.UUID (UUID)
import System.Posix.Files (getFileStatus, fileID, isDirectory)
import System.Posix.Types (CIno(..))

$(deriveJSON defaultOptions ''UUID)
$(deriveJSON defaultOptions ''SubvolInfo)

data Subvol =
  Subvol
  { subvolId :: SubvolId
  , subvolParentId :: SubvolId
  , subvolInodeNum :: InodeNum
  , subvolName :: FilePath
  , subvolVolumePath :: FilePath
  , subvolPath :: FilePath
  , subvolInfo :: SubvolInfo
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Subvol)

data SubvolTable =
  SubvolTable
  { tableSubvols :: [Subvol]
  , tableSubvolsById :: Map SubvolId Subvol
  , tableSubvolsByUUID :: Map UUID Subvol
  , tableSubvolsByReceivedUUID :: Map UUID Subvol
  } deriving (Eq)

data SnapshotStore =
  SnapshotStore
  { storeSubvolTable :: SubvolTable
  , storePath :: FilePath
  } deriving (Eq)

$(deriveJSON defaultOptions ''SnapshotStore)

mkSubvolTable :: [Subvol] -> SubvolTable
mkSubvolTable subvols =
  SubvolTable
  { tableSubvols = sortBy dateCompare subvols
  , tableSubvolsById = fromList $ map mkIdPair subvols
  , tableSubvolsByUUID = fromList $ catMaybes $ map mkUUIDPair subvols
  , tableSubvolsByReceivedUUID = fromList $ catMaybes $ map mkReceivedUUIDPair subvols
  }
  where
    date = siOTime . subvolInfo
    dateCompare left right = compare (date left) (date right)
    mkIdPair subvol = (subvolId subvol, subvol)
    mkUUIDPair subvol = fmap (\uuid -> (uuid, subvol)) . siUuid . subvolInfo $ subvol
    mkReceivedUUIDPair subvol = fmap (\uuid -> (uuid, subvol)) . siReceivedUuid . subvolInfo $ subvol

mkSnapshotStore :: FilePath -> [Subvol] -> SnapshotStore
mkSnapshotStore path subvols =
  SnapshotStore
  { storeSubvolTable = mkSubvolTable subvols
  , storePath = path
  }

storeSnapshots :: SnapshotStore -> [Subvol]
storeSnapshots = tableSubvols . storeSubvolTable

newtype Local a = Local { fromLocal :: a }
newtype Remote a = Remote { fromRemote :: a }

compareStores :: Local SnapshotStore -> Remote SnapshotStore -> ([Local Subvol], [(Local Subvol, Remote Subvol)])
compareStores local remote = (localOnly, common)
  where commonUUIDs = intersectionWith combine localUUIDMap remoteUUIDMap
        combine l r = (Local l, Remote r)
        common = elems commonUUIDs
        localOnlyUUIDs = difference localUUIDMap remoteUUIDMap
        localOnly = map Local $ filter isNotReceived $ elems localOnlyUUIDs
        isNotReceived = not . isJust . siReceivedUuid . subvolInfo
        localStore = fromLocal local
        remoteStore = fromRemote remote
        localUUIDMap = tableSubvolsByUUID $ storeSubvolTable localStore
        remoteUUIDMap = tableSubvolsByReceivedUUID $ storeSubvolTable remoteStore

loadSnapshotStoreWithFilter :: MonadIO m => FilePath -> (Subvol -> m Bool) -> m SnapshotStore
loadSnapshotStoreWithFilter path filter = do
  findSubvolsInPath path >>= filterM filter >>= return . mkSnapshotStore path

defaultSnapshotStoreFilter :: MonadIO m => Subvol -> m Bool
defaultSnapshotStoreFilter = return . isRight . validateSubvol

loadSnapshotStore :: MonadIO m => FilePath -> m SnapshotStore
loadSnapshotStore path = loadSnapshotStoreWithFilter path defaultSnapshotStoreFilter

instance ToJSON SubvolTable where
  toJSON = toJSON . tableSubvols
  toEncoding = toEncoding . tableSubvols

instance FromJSON SubvolTable where
  parseJSON = withArray "SubvolTable" parse
    where
      parse :: Array -> Parser SubvolTable
      parse array = mkSubvolTable <$> foldr' parseAndCons (return []) array
      parseAndCons :: Value -> Parser [Subvol] -> Parser [Subvol]
      parseAndCons value existingParser = (:) <$> parseJSON value <*> existingParser

convertCIno :: CIno -> InodeNum
convertCIno (CIno no) = no

getPathToFileRelativeToSubvol :: MonadIO m => FilePath -> m (SubvolId, FilePath)
getPathToFileRelativeToSubvol path = do
  let (containingDir, fileName) = splitFileName path
  when ("" == fileName) $ do
    liftIO $ ioError $ userError $ path ++ " is not a directory"

  (subvolId, directoryPath) <- getPathRelativeToSubvol containingDir
  return $ (subvolId, directoryPath </> fileName)

getPathRelativeToSubvol :: MonadIO m => FilePath -> m (SubvolId, FilePath)
getPathRelativeToSubvol path = do
  status <- liftIO $ getFileStatus path
  if isDirectory status
    then liftIO $ lookupInode path 0 (convertCIno $ fileID status)
    else getPathToFileRelativeToSubvol path

inodePathStripPrefix :: FilePath -> FilePath -> Maybe FilePath
inodePathStripPrefix prefix path = let
  relative = makeRelative prefix path
  in if isRelative relative then Just relative else Nothing

findSubvolsInPath :: MonadIO m => FilePath -> m [Subvol]
findSubvolsInPath path = do
  (parentSubvolId, pathRelativeToParentSubvol) <- getPathRelativeToSubvol path
  children <- liftIO $ childSubvols path parentSubvolId
  liftM catMaybes $ forM children $ \(childId, childInodeNum, childName) -> do
    info <- liftIO $ getSubvolInfo path childId
    (_, pathToChild) <- liftIO $ lookupInode path parentSubvolId childInodeNum
    case inodePathStripPrefix pathRelativeToParentSubvol pathToChild of
      Nothing -> return Nothing
      Just relativeSubvolPath -> do
        return $ Just $
          Subvol
          { subvolId = childId
          , subvolParentId = parentSubvolId
          , subvolInodeNum = childInodeNum
          , subvolName = childName
          , subvolVolumePath = path
          , subvolPath = path </> relativeSubvolPath </> childName
          , subvolInfo = info
          }

lookupSubvolPathToParent :: MonadIO m => Subvol -> m FilePath
lookupSubvolPathToParent subvol = do
  (_, path) <- liftIO $ lookupInode (subvolVolumePath subvol) (subvolParentId subvol) (subvolInodeNum subvol)
  return $ path </> subvolName subvol

validateSubvol :: Subvol -> Either String Subvol
validateSubvol subvol = do
  let info = subvolInfo subvol
  let ensure condition description =
        if condition
        then return subvol
        else Left description
  -- System.Linux.Btrfs seems to always claim subvolumes are readwrite
  -- ensure (siReadOnly info)
  --   "Snapshot is not readonly"
  ensure (isJust $ siParSnapGen info)
    "Subvolume is not a snapshot"
  ensure (siParSnapGen info == Just (siGeneration info))
    "Subvolume has been modified"
