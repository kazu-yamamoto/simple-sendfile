module Network.Sendfile.Fallback (sendfile) where

import Network.Socket
import Network.Sendfile.Types

sendfile :: Socket -> FilePath -> FileRange -> IO ()
sendfile = undefined
