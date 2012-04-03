module Network.Sendfile.Fallback (sendfile) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary as EB
import Network.Sendfile.Types
import Network.Socket
import qualified Network.Socket.ByteString as SB

{-|
   Sendfile emulation using enumerator.

   - Used system calls: open(), stat(), read(), send() and close().
-}
sendfile :: Socket -> FilePath -> FileRange -> IO () -> IO ()
sendfile s fp EntireFile hook =
    runResourceT $ sourceFile fp $$ sinkSocket s hook
sendfile s fp (PartOfFile off len) hook =
    runResourceT $ EB.sourceFileRange fp (Just off) (Just len) $$ sinkSocket s hook

sinkSocket :: MonadResource m => Socket -> IO () -> Sink ByteString m ()
sinkSocket s hook = NeedInput push close
  where
    push bs = PipeM a close'
      where a = do
                  liftIO (SB.sendAll s bs)
                  liftIO hook
                  return (NeedInput push close)
    close' = return ()
    close  = lift close'
