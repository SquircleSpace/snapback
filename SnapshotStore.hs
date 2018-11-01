module SnapshotStore (Subvol(..), findSubvolsInPath) where
import Control.Monad (when, liftM, forM)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import System.FilePath ((</>), splitFileName)
import System.Linux.Btrfs (InodeNum, SubvolId, SubvolInfo(..), lookupInode, childSubvols)
import System.Posix.Files (getFileStatus, fileID, isDirectory)
import System.Posix.Types (CIno(..))

data Subvol =
  Subvol
  { subvolId :: SubvolId
  , subvolParentId :: SubvolId
  , subvolInodeNum :: InodeNum
  , subvolName :: FilePath
  , subvolVolumePath :: FilePath
  , subvolInfo :: SubvolInfo
  } deriving (Eq, Show)

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
