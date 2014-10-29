import CGroup
import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Data.List (intercalate)

-- root of the cgroup filesystem; change if necessary
config :: Config
config = Config "/sys/fs/cgroup"

{- process a single request, and return the response.

Input requests consist of space-separated words, the first of which is
CREATECG, MOVEPID, or LISTPIDS, and the rest of which are arguments for
the request.

Output responses consist of either "OK " followed by space-separated
responses to the request, or "ERROR " followed by an error message.

The argument to CREATECG is the name of a control group; there is no
response.

The arguments to MOVEPID are a numeric process ID and the name of a
control group; there is no response.

The argument to LISTPIDS is the name of a control group; the response is
a set of PIDs.
-}
process_request :: String -> IO String
process_request r = either ("ERROR " ++) ("OK " ++) <$>
                    (runCM config . pr . words) r
  where
    pr ["CREATECG", cg] = createCGroup cg >> return ""
    pr ["MOVEPID", pid, cg] = movePID (read pid) cg >> return ""
    pr ["LISTPIDS", cg] = intercalate " " . map show <$> listPIDs cg
    pr _ = fail "unrecognized command or invalid arguments"

main :: IO ()
main = getContents >>= mapM_ (putStrLn <=< process_request) . lines
