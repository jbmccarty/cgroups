{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
import CGroup
import Control.DeepSeq
import Control.Exception (IOException)
import Control.Monad.Catch (handle)
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (intercalate)
import GHC.IO.Exception (ioe_type, IOErrorType(..))
import Network.CGI.Monad
import Network.CGI.Protocol
import Network.FastCGI

-- the CGI library is missing some useful instances...
instance NFData CGIResult where
  rnf CGINothing = ()
  rnf (CGIOutput s) = rnf s

instance MonadCGI m => MonadCGI (ExceptT e m) where
  cgiAddHeader n s = lift $ cgiAddHeader n s
  cgiGet = lift . cgiGet

instance MonadCGI m => MonadCGI (ReaderT r m) where
  cgiAddHeader n s = lift $ cgiAddHeader n s
  cgiGet = lift . cgiGet

-- an error consists of a return code and some messages
type Error = (Int, String, [String])

-- root of the cgroup filesystem; change if necessary
config :: Config
config = Config "/sys/fs/cgroup"

-- 400 invalid request error
invalid :: String -> Error
invalid s = (400, "Invalid Request", [s])

-- get the value of a parameter, returning a 400 error if it's not present
getValue :: (MonadError Error m, MonadCGI m) => String -> m String
getValue s = getInput s >>=
             maybe (throwError . invalid $ "Missing parameter: " ++ s)
                   return

-- interpret the value of a parameter, returning a 400 error if it's not
-- valid
readValue :: (Read a, MonadError Error m, MonadCGI m) => String -> m a
readValue s = do
  s' <- getValue s
  let v = reads s'
  case v of
    [(v', "")] -> return v'
    _ -> throwError . invalid $ "Invalid value for " ++ s ++ ": " ++ s'

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
processRequest :: CGI CGIResult
-- here we use ExceptT for early-out behavior, and ReaderT to pass the
-- cgroup root location.
processRequest = handleOutput . flip runReaderT config $ do
  cmd <- getValue "command"
  case cmd of
    "createcgroup" -> getValue "cgroup" >>= createCGroup >> outputNothing
    "movepid"  -> do
      pid <- readValue "pid"
      cg <- getValue "cgroup"
      movePID pid cg
      outputNothing
    "listpids" -> getValue "cgroup" >>= listPIDs >>=
                  output . (++ "\n") . intercalate " " . map show
    _ -> throwError . invalid $ "Invalid command: " ++ cmd

-- check output for error codes and client-caused exceptions (remaining
-- exceptions are caught by handleErrors below and result in a 500
-- Internal Server Error)
handleOutput :: ExceptT Error (CGIT IO) CGIResult -> CGI CGIResult
handleOutput r = handleExceptions (runExceptT r) >>=
                 either handleEither return
  where
    -- convert exceptions to Left values to prevent them escaping in a
    -- thunk (even if they were already Left values)
    handleExceptions r = h1 handleIO . h1 handlePath $ do
      res <- r
      deepseq res $ return res -- make sure any exception is forced
    h1 f = handle (return . Left . f) -- handle one exception
    handleIO (e :: IOException) = case ioe_type e of
      NoSuchThing -> (404, "Not Found", [show e])
      _ -> (403, "Forbidden", [show e])
    handlePath (PathException p) = invalid $
      "\"..\" not allowed as a path component: " ++ p

    -- finally convert any errors to output
    handleEither (c, e, es) = outputError c e es

main :: IO ()
main = runFastCGIorCGI $ handleErrors processRequest
