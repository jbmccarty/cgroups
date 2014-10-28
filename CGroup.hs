-- This module uses the filesystem to manipulate cgroups. Errors are
-- reported as strings.

module CGroup where
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad.Reader
import System.Directory
import System.FilePath

-- process ID
type PID = Integer

-- configuration data
data Config = Config {
  cgroup_root :: FilePath -- root of the cgroup filesystem;
                          -- shouldn't end in "/"
}

-- monad for using configuration data
type CM a = ReaderT Config IO a

-- extract the value from the monad, converting any exceptions to
-- strings
-- Perhaps we should only catch certain exceptions so as not to hide
-- programming errors.
runCM :: Config -> CM a -> IO (Either String a)
runCM c m = left showE <$> try (runReaderT m c) where
  showE :: SomeException -> String
  showE = show

-- throw an error if a path contains any ".." components, to prevent
-- accessing files we shouldn't. The administrator is responsible for
-- not creating other links to directories we shouldn't access.
-- Alternative: use System.FilePath.Canonical.canonicalFilePath to
-- check if we left cgroup_root.
sanitize :: FilePath -> CM ()
sanitize p = when (any (== "..") $ splitDirectories p) $
             fail "\"..\" not allowed in paths"

-- filesystem path of a cgroup
path :: FilePath -> CM FilePath
path cg = do
  sanitize cg
  (++ ("/" ++ cg)) . cgroup_root <$> ask

-- filesystem path of the tasks file for a cgroup
tasks :: FilePath -> CM FilePath
tasks cg = (++ "/tasks") <$> path cg

-- create a cgroup
createCGroup :: FilePath -> CM ()
createCGroup cg = path cg >>= liftIO . createDirectory

-- place a process into a cgroup
movePID :: PID -> FilePath -> CM ()
movePID pid cg = tasks cg >>= liftIO . flip writeFile (show pid ++ "\n")

listPIDs :: String -> CM [Integer]
listPIDs cg = tasks cg >>= liftIO . fmap (map read . lines) . readFile

{- Not implemented, but it wouldn't be too difficult:
 - creating/deleting a hierarchy attached to given controllers
 - deleting cgroups
 - listing cgroups/hierarchies/controllers
 - listing/setting cgroup attributes
 -}
