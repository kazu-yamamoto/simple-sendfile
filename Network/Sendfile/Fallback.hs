module Network.Sendfile.Fallback (sendfile) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary as EB
import Network.Sendfile.Types
import Network.Socket
import qualified Network.Socket.ByteString as SB

{-|
   Sendfile emulation using conduit.

   - Used system calls: open(), stat(), read(), send() and close().
-}
sendfile :: Socket -> FilePath -> FileRange -> IO () -> IO ()
sendfile s fp EntireFile hook =
    runResourceT $ sourceFile fp $$ sinkSocket s hook
sendfile s fp (PartOfFile off len) hook =
    runResourceT $ EB.sourceFileRange fp (Just off) (Just len) $$ sinkSocket s hook

-- See sinkHandle.
sinkSocket :: MonadIO m => Socket -> IO () -> Sink ByteString m ()
sinkSocket s hook = NeedInput push close
  where
    push bs = flip PipeM (return ()) $ do
        liftIO (SB.sendAll s bs)
        liftIO hook
        return (NeedInput push close)
    close = return ()
