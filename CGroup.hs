-- This module uses the filesystem to manipulate cgroups.

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module CGroup (PathException(..), PID, Config(..), createCGroup, movePID,
  listPIDs) where
import Control.Monad.Reader
import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import System.Directory (createDirectory)
import System.FilePath (splitDirectories)

-- not necessary once Applicative becomes a superclass of Monad
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
infixl 4 <$>

-- process ID
type PID = Integer

-- configuration data
data Config = Config {
  cgroup_root :: FilePath -- root of the cgroup filesystem;
                          -- shouldn't end in "/"
}

-- exception for an invalid path
data PathException = PathException FilePath deriving (Typeable, Show)
instance Exception PathException

-- throw an error if a path contains any ".." components, to prevent
-- accessing files we shouldn't. The administrator is responsible for
-- not creating other links to directories we shouldn't access.
-- Alternative: use System.FilePath.Canonical.canonicalFilePath to
-- check if we left cgroup_root.
sanitize :: MonadReader Config m => FilePath -> m ()
sanitize p = when (any (== "..") $ splitDirectories p) $
             throw $ PathException p

-- filesystem path of a cgroup
path :: MonadReader Config m => FilePath -> m FilePath
path cg = do
  sanitize cg
  (++ ("/" ++ cg)) . cgroup_root <$> ask

-- filesystem path of the tasks file for a cgroup
tasks :: MonadReader Config m => FilePath -> m FilePath
tasks cg = (++ "/tasks") <$> path cg

-- create a cgroup
-- Sanity check: is the parent directory a cgroup?
createCGroup :: (MonadReader Config m, MonadIO m) => FilePath -> m ()
createCGroup cg = path cg >>= liftIO . createDirectory

-- place a process into a cgroup
-- Sanity check: is cg actually a cgroup?
movePID :: (MonadReader Config m, MonadIO m) => PID -> FilePath -> m ()
movePID pid cg = tasks cg >>= liftIO . flip writeFile (show pid ++ "\n")

-- list the pids in a cgroup
-- Sanity check: is cg actually a cgroup?
listPIDs :: (MonadReader Config m, MonadIO m) => FilePath -> m [Integer]
listPIDs cg = tasks cg >>= liftIO . fmap (map read . lines) . readFile

{- Not implemented, but it wouldn't be too difficult:
 - creating/deleting a hierarchy attached to given controllers
 - deleting cgroups
 - listing cgroups/hierarchies/controllers
 - listing/setting cgroup attributes
 -}
