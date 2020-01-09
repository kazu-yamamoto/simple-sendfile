{-# LANGUAGE CPP #-}
module Network.Sendfile.Fallback (
    sendfile
    , sendfileFd
    , sendfileWithHeader
    , sendfileFdWithHeader
    ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary as EB
import Network.Sendfile.Types
import Network.Socket
import Network.Socket.ByteString
import qualified Network.Socket.ByteString as SB
import Control.Monad.Trans.Resource (runResourceT)
import System.Posix.Types

-- |
-- Sendfile emulation using conduit.
-- Used system calls:
--
--  - Used system calls: open(), stat(), read(), send() and close().

sendfile :: Socket -> FilePath -> FileRange -> IO () -> IO ()
sendfile sock path EntireFile hook =
    runResourceT $ sourceFile path $$ sinkSocket sock hook
sendfile sock path (PartOfFile off len) hook =
    runResourceT $ EB.sourceFileRange path (Just off) (Just len) $$ sinkSocket sock hook

sendfileFd :: Socket -> Fd -> FileRange -> IO () -> IO ()
sendfileFd _sock _fd _range _hook = undefined

-- See sinkHandle.
sinkSocket :: MonadIO m => Socket -> IO () -> Sink ByteString m ()
#if MIN_VERSION_conduit(0,5,0)
sinkSocket s hook = awaitForever $ \bs -> liftIO $ SB.sendAll s bs >> hook
#else
sinkSocket s hook = NeedInput push close
  where
    push bs = flip PipeM (return ()) $ do
        liftIO (SB.sendAll s bs)
        liftIO hook
        return (NeedInput push close)
    close = return ()
#endif

-- |
-- Sendfile emulation using conduit.
-- Used system calls:
--
--  - Used system calls: open(), stat(), read(), writev(), send() and close().

sendfileFdWithHeader :: Socket -> Fd -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileFdWithHeader _sock _fd _range _hook _hdr = undefined

sendfileWithHeader :: Socket -> FilePath -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileWithHeader sock path range hook hdr = do
    sendMany sock hdr
    sendfile sock path range hook
