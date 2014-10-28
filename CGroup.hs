module CGroup where
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad.Reader
import System.Directory

-- process ID
type PID = Integer

-- configuration data
data Config = Config {
  cgroup_root :: FilePath
}

-- change this if necessary
defaultConfig :: Config
defaultConfig = Config "/sys/fs/cgroup"

-- monad for using configuration data and reporting errors
type M a = ReaderT Config IO a

-- extract the value from the monad, converting any exceptions to
-- strings
-- Perhaps we should only catch file access exceptions, since anything
-- else is probably a bug.
runM :: Config -> M a -> IO (Either String a)
runM c m = left showE <$> try (runReaderT m c) where
  showE :: SomeException -> String
  showE = show

-- throw an error if a path contains any ".." components, to prevent
-- accessing files we shouldn't. The administrator is responsible for
-- not creating other links to directories we shouldn't access.
-- Alternative: use System.FilePath.Canonical.canonicalFilePath to
-- check if we left cgroup_root.
sanitize :: FilePath -> M ()
sanitize p = when (any (== "..") $ splitDirectories p) $
             fail "\"..\" not allowed in paths"

-- filesystem path of a cgroup
path :: FilePath -> M FilePath
path cg = do
  sanitize cg
  (++ ("/" ++ cg)) . cgroup_root <$> ask

-- filesystem path of the tasks file for a cgroup
tasks :: FilePath -> M FilePath
tasks cg = (++ "/tasks") <$> path cg

-- create a cgroup
createCG :: FilePath -> M ()
createCG cg = path cg >>= liftIO . createDirectory

-- place a process into a cgroup
movePID :: PID -> FilePath -> M ()
movePID pid cg = tasks cg >>= liftIO . flip writeFile (show pid)

listPIDs :: String -> M [Integer]
listPIDs cg = tasks cg >>= liftIO . fmap (map read . lines) . readFile
