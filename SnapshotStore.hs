{-# LANGUAGE TemplateHaskell #-}

module SnapshotStore (Subvol(..), SnapshotStore, storeSnapshots, mkSnapshotStore, loadSnapshotStore, loadSnapshotStoreWithFilter, findSubvolsInPath, lookupSubvolPathToParent, validateSubvol) where

import Control.Monad (when, liftM, forM, filterM)
import Data.Aeson (ToJSON, FromJSON, Array, Value, toJSON, toEncoding, parseJSON, withArray)
import Data.Aeson.Types (Parser)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Either (isRight)
import Data.Foldable (foldr')
import Data.List (isPrefixOf, sortBy)
import Data.Map.Strict (Map, fromList)
import Data.Maybe (catMaybes, isJust)
import System.FilePath ((</>), splitFileName)
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
  , subvolInfo :: SubvolInfo
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Subvol)

data SnapshotStore =
  SnapshotStore
  { storeSnapshots :: [Subvol]
  , storeSnapshotsById :: Map SubvolId Subvol
  } deriving (Eq)

mkSnapshotStore :: [Subvol] -> SnapshotStore
mkSnapshotStore subvols =
  SnapshotStore
  { storeSnapshots = sortBy dateCompare subvols
  , storeSnapshotsById = fromList $ map mkPair subvols
  }
  where
    date = siOTime . subvolInfo
    dateCompare left right = compare (date left) (date right)
    mkPair subvol = (subvolId subvol, subvol)

loadSnapshotStoreWithFilter :: FilePath -> (Subvol -> IO Bool) -> IO SnapshotStore
loadSnapshotStoreWithFilter path filter = do
  findSubvolsInPath path >>= filterM filter >>= return . mkSnapshotStore

defaultSnapshotStoreFilter :: Subvol -> IO Bool
defaultSnapshotStoreFilter = return . isRight . validateSubvol

loadSnapshotStore :: FilePath -> IO SnapshotStore
loadSnapshotStore path = loadSnapshotStoreWithFilter path defaultSnapshotStoreFilter

instance ToJSON SnapshotStore where
  toJSON = toJSON . storeSnapshots
  toEncoding = toEncoding . storeSnapshots

instance FromJSON SnapshotStore where
  parseJSON = withArray "SnapshotStore" parse
    where
      parse :: Array -> Parser SnapshotStore
      parse array = mkSnapshotStore <$> foldr' parseAndCons (return []) array
      parseAndCons :: Value -> Parser [Subvol] -> Parser [Subvol]
      parseAndCons value existingParser = (:) <$> parseJSON value <*> existingParser

convertCIno :: CIno -> InodeNum
convertCIno (CIno no) = no

getPathToFileRelativeToSubvol :: FilePath -> IO (SubvolId, FilePath)
getPathToFileRelativeToSubvol path = do
  let (containingDir, fileName) = splitFileName path
  when ("" == fileName) $ do
    ioError $ userError $ path ++ " is not a directory"

  (subvolId, directoryPath) <- getPathRelativeToSubvol containingDir
  return $ (subvolId, directoryPath </> fileName)

getPathRelativeToSubvol :: FilePath -> IO (SubvolId, FilePath)
getPathRelativeToSubvol path = do
  status <- getFileStatus path
  if isDirectory status
    then lookupInode path 0 (convertCIno $ fileID status)
    else getPathToFileRelativeToSubvol path

inodePathIsPrefixOfPath :: FilePath -> FilePath -> Bool
inodePathIsPrefixOfPath inodePath otherInodePath =
  cooked inodePath `isPrefixOf` cooked otherInodePath
  where cooked path = if path == "/" then "" else path

findSubvolsInPath :: FilePath -> IO [Subvol]
findSubvolsInPath path = do
  (parentSubvolId, pathRelativeToParentSubvol) <- getPathRelativeToSubvol path
  children <- childSubvols path parentSubvolId
  liftM catMaybes $ forM children $ \(childId, childInodeNum, childName) -> do
    info <- getSubvolInfo path childId
    let childSubvol =
          Subvol
          { subvolId = childId
          , subvolParentId = parentSubvolId
          , subvolInodeNum = childInodeNum
          , subvolName = childName
          , subvolVolumePath = path
          , subvolInfo = info
          }
    (_, pathToChild) <- lookupInode path parentSubvolId (subvolInodeNum childSubvol)
    return $ if pathRelativeToParentSubvol `inodePathIsPrefixOfPath` pathToChild
             then Just childSubvol
             else Nothing

lookupSubvolPathToParent :: Subvol -> IO FilePath
lookupSubvolPathToParent subvol = do
  (_, path) <- lookupInode (subvolVolumePath subvol) (subvolParentId subvol) (subvolInodeNum subvol)
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
