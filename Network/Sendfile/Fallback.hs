module Network.Sendfile.Fallback (
    sendfile
  , sendfileWithHeader
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Binary as EB
import Network.Sendfile.Types
import Network.Socket
import qualified Network.Socket.ByteString as SB
import System.IO (Handle, IOMode(ReadMode), withBinaryFile)

-- |
-- Sendfile emulation using conduit.
--
--  - Used system calls: open(), stat(), read(), send() and close().

sendfile :: Socket -> FilePath -> FileRange -> IO () -> IO ()
sendfile sock path range hook = withBinaryFile path ReadMode $ \h ->
    sendHandle sock h range hook

-- |
-- Sendfile emulation using conduit.
--
--  - Used system calls: open(), stat(), read(), writev(), send() and close().
--
-- The file is opened before the header is sent, so nothing is sent
-- if the file cannot be opened.

sendfileWithHeader :: Socket -> FilePath -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileWithHeader sock path range hook hdr = withBinaryFile path ReadMode $ \h -> do
    SB.sendMany sock hdr
    sendHandle sock h range hook

sendHandle :: Socket -> Handle -> FileRange -> IO () -> IO ()
sendHandle sock h range hook = runConduit $ source .| sinkSocket sock hook
  where
    source = case range of
        EntireFile         -> EB.sourceHandle h
        PartOfFile off len -> EB.sourceHandleRange h (Just off) (Just len)

-- See sinkHandle.
sinkSocket :: MonadIO m => Socket -> IO () -> ConduitT ByteString Void m ()
sinkSocket s hook = awaitForever $ \bs -> liftIO $ SB.sendAll s bs >> hook
