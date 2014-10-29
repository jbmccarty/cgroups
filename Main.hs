{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
import CGroup
import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.List (intercalate)
import Network.FastCGI
import Network.CGI.Monad
import Control.DeepSeq

-- an error consists of a return code and a message
type Error = (Int, String, [String])

-- customized ExceptT to make "fail" produce a 400 error
newtype ET m a = ET { unET :: ExceptT Error m a }
  deriving (Applicative, Functor, MonadIO, MonadError Error, MonadTrans)

runET :: ET m a -> m (Either Error a)
runET = runExceptT . unET

instance Monad m => Monad (ET m) where
  return = ET . return
  x >>= f = ET $ unET x >>= unET . f
  fail s = ET $ invalid s

instance MonadCGI m => MonadCGI (ET m) where
  cgiAddHeader n s = lift $ cgiAddHeader n s
  cgiGet = lift . cgiGet

-- root of the cgroup filesystem; change if necessary
config :: Config
config = Config "/sys/fs/cgroup"

runCG :: ReaderT Config m a -> m a
runCG = flip runReaderT config

-- produce a 400 Invalid Request response
invalid :: MonadError Error m => String -> m a
invalid s = throwError (400, "Invalid Request", [s])

-- get the value of a parameter, returning a 400 error if it's not present
getValue :: (MonadError Error m, MonadCGI m) => String -> m String
getValue s = getInput s >>=
             maybe (invalid $ "Missing parameter: " ++ s) return

-- interpret the value of a parameter, returning a 400 error if it's not
-- valid
readValue :: (Read a, MonadError Error m, MonadCGI m) => String -> m a
readValue s = do
  s' <- getValue s
  let v = reads s'
  case v of
    [(v', "")] -> return v'
    _ -> invalid $ "Invalid value for " ++ s ++ ": " ++ s'

{- process a single request, and output the response.

Valid parameters and values:
  The "command" parameter specifies the action to take. Its value can be
  "createcg", "movepid", or "listpids".

  For "createcg", there must be a parameter "cgroup", whose value is the
  name of the cgroup to create. Only POST is allowed.

  For "movepid", there must be a parameter "pid" whose value is the pid
  to move, and "cgroup", whose value is the name of the cgroup to move
  it into. Only POST is allowed.

  For "listpids", there must be a parameter "cgroup", whose value is the
  name of the cgroup to list. Only HEAD and GET are allowed. A
  space-separated list of pids is returned in the response.
-}
process_request :: CGI CGIResult
-- here we use MaybeT to make pattern match failures result in Nothing,
-- and ExceptT for early-out behavior
process_request = handle_output . runET $ do
  -- the non-exhaustive pattern matching here is intentional: any
  -- invalid command syntax results in a pattern match failure, which
  -- gets interpreted as a 400 invalid request
  cmd <- getValue "command"
  case cmd of
    "createcgroup" -> do
      cg <- getValue "cgroup"
      res <- runCG $ createCGroup cg
      deepseq res $ outputNothing
      -- check res for errors
    "movepid"  -> do
      pid <- readValue "pid"
      cg <- getValue "cgroup"
      res <- runCG $ movePID pid cg
      deepseq res $ outputNothing
    "listpids" -> do
      cg <- getValue "cgroup"
      res <- runCG $ listPIDs cg
      -- check res for errors
      let res' = (++ "\n") . intercalate " " . map show $ res
      deepseq res' $ output res'
    _ -> invalid $ "Invalid command: " ++ cmd

-- check output for error codes (remaining exceptions are caught by
-- handleErrors below and result in a 500 Internal Server Error)
handle_output :: CGI (Either Error CGIResult) -> CGI CGIResult
handle_output r = r >>= either h return where
  h (c, e, es) = outputError c e es

main :: IO ()
main = runFastCGIorCGI $ handleErrors process_request
