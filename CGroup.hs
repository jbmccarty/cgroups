-- This module uses the filesystem to manipulate cgroups. Errors are
-- reported as strings.

module CGroup where
import Control.Applicative ((<$>))
import Control.Arrow (left)
import Control.DeepSeq (NFData, deepseq)
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import System.Directory (createDirectory)
import System.FilePath (splitDirectories)

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
runCM :: NFData a => Config -> CM a -> IO (Either String a)
runCM c m = left show <$> tryE r where
  r = do
    res <- runReaderT m c
    deepseq res $ return res
    -- laziness makes preventing exceptions from escaping the monad
    -- tricky: we need to force everything that gets returned
  tryE :: IO a -> IO (Either SomeException a) -- for type inference
  tryE = try

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
-- Sanity check: is the parent directory a cgroup?
createCGroup :: FilePath -> CM ()
createCGroup cg = path cg >>= liftIO . createDirectory

-- place a process into a cgroup
-- Sanity check: is cg actually a cgroup?
movePID :: PID -> FilePath -> CM ()
movePID pid cg = tasks cg >>= liftIO . flip writeFile (show pid ++ "\n")

-- list the pids in a cgroup
-- Sanity check: is cg actually a cgroup?
listPIDs :: String -> CM [Integer]
listPIDs cg = tasks cg >>= liftIO . fmap (map read . lines) . readFile

{- Not implemented, but it wouldn't be too difficult:
 - creating/deleting a hierarchy attached to given controllers
 - deleting cgroups
 - listing cgroups/hierarchies/controllers
 - listing/setting cgroup attributes
 -}
