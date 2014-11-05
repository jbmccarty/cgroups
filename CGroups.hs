-- Module for manipulating Linux cgroups via the filesystem.
-- Copyright 2014 Jason McCarty

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module CGroups (PathException(..), PID, Config(..), createCGroup,
  movePID, listPIDs) where
import Control.Monad.Reader
import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import System.Directory (createDirectory)
import System.FilePath (splitDirectories, (</>))

-- not necessary once Applicative becomes a superclass of Monad
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
infixl 4 <$>

-- process ID
type PID = Integer

-- configuration data
newtype Config = Config {
  cgroupRoot :: FilePath -- root of the cgroup filesystem;
                          -- shouldn't end in "/"
}

-- exception for an invalid path
newtype PathException = PathException FilePath deriving (Typeable, Show)
instance Exception PathException

-- throw an error if a path contains any ".." components, to prevent
-- accessing files we shouldn't. The administrator is responsible for
-- not creating other links to directories we shouldn't access.
-- Alternative: use System.FilePath.Canonical.canonicalFilePath to
-- check if we left cgroupRoot.
sanitize :: MonadReader Config m => FilePath -> m ()
sanitize p = when (".." `elem` splitDirectories p) $
             throw $ PathException p

-- filesystem path of a cgroup
path :: MonadReader Config m => FilePath -> m FilePath
path cg = sanitize cg >> (</> cg) . cgroupRoot <$> ask

-- filesystem path of the tasks file for a cgroup
tasks :: MonadReader Config m => FilePath -> m FilePath
tasks cg = (</> "tasks") <$> path cg

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
listPIDs cg = tasks cg >>= liftIO . fmap (map read . words) . readFile
-- this returns a 500 error if read fails, but that's arguably correct
-- as the file should only contain numbers

{- Not implemented, but it wouldn't be too difficult:
 - creating/deleting a hierarchy attached to given controllers
 - deleting cgroups
 - listing cgroups/hierarchies/controllers
 - listing/setting cgroup attributes
 -}
