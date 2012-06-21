{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Sendfile.MacOS (
    sendfile
  , sendfileWithHeader
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Network.Sendfile.IOVec
import Network.Sendfile.Types
import Network.Socket
import System.Posix.IO
import System.Posix.Types (Fd(..), COff(..))
import qualified Data.ByteString as BS

#include <sys/types.h>

{-|
   Simple binding for sendfile() of MacOS.

   - Used system calls: open(), sendfile(), and close().

   The fourth action argument is called when a file is sent as chunks.
   Chucking is inevitable if the socket is non-blocking (this is the
   default) and the file is large. The action is called after a chunk
   is sent and bofore waiting the socket to be ready for writing.
-}
sendfile :: Socket -> FilePath -> FileRange -> IO () -> IO ()
sendfile sock path range hook = bracket setup teardown $ \fd ->
    alloca $ \lenp -> case range of
        EntireFile -> do
            poke lenp 0
            sendloop dst fd 0 lenp hook
        PartOfFile off len -> do
            let off' = fromInteger off
            poke lenp (fromInteger len)
            sendloop dst fd off' lenp hook
  where
    setup = openFd path ReadOnly Nothing defaultFileFlags
    teardown = closeFd
    dst = Fd $ fdSocket sock

sendloop :: Fd -> Fd -> COff -> Ptr COff -> IO () -> IO ()
sendloop dst src off lenp hook = do
    len <- peek lenp
    rc <- c_sendfile src dst off lenp nullPtr
    when (rc /= 0) $ do
        errno <- getErrno
        if errno `elem` [eAGAIN, eINTR] then do
            sent <- peek lenp
            if len == 0 then
                poke lenp 0 -- Entire
              else
                poke lenp (len - sent)
            hook
            threadWaitWrite dst
            sendloop dst src (off + sent) lenp hook
          else
            throwErrno "Network.SendFile.MacOS.sendloop"

c_sendfile :: Fd -> Fd -> COff -> Ptr COff -> Ptr SfHdtr -> IO CInt
c_sendfile fd s offset lenp hdrp = c_sendfile' fd s offset lenp hdrp 0

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile'
    :: Fd -> Fd -> COff -> Ptr COff -> Ptr SfHdtr -> CInt -> IO CInt

----------------------------------------------------------------

sendfileWithHeader :: Socket -> FilePath -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileWithHeader sock path range hook hdr = bracket setup teardown $ \fd -> do
    alloca $ \lenp -> case range of
        EntireFile -> do
            poke lenp 0
            mrc <- sendloopHeader dst fd 0 lenp hook hdr
            case mrc of
                Just (newoff, _) -> do
                    poke lenp 0
                    sendloop dst fd newoff lenp hook
                _ -> return ()
        PartOfFile off len -> do
            let off' = fromInteger off
            poke lenp $ fromInteger len + hlen
            mrc <- sendloopHeader dst fd off' lenp hook hdr
            case mrc of
                Just (newoff, Just newlen) -> do
                    poke lenp newlen
                    sendloop dst fd newoff lenp hook
                _ -> return ()
  where
    setup = openFd path ReadOnly Nothing defaultFileFlags
    teardown = closeFd
    dst = Fd $ fdSocket sock
    hlen = fromIntegral . sum . map BS.length $ hdr

sendloopHeader :: Fd -> Fd -> COff -> Ptr COff -> IO () -> [ByteString] -> IO (Maybe (COff, Maybe COff))
sendloopHeader dst src off lenp hook hdr = do
    len <- peek lenp
    rc <- withSfHdtr hdr $ c_sendfile src dst off lenp
    if rc == 0 then
        return Nothing
      else do
        errno <- getErrno
        if errno `elem` [eAGAIN, eINTR] then do
            sent <- peek lenp
            if sent >= hlen then do
                let newoff = off + sent - hlen
                if len == 0 then
                    return $ Just (newoff, Nothing)
                  else
                    return $ Just (newoff, Just (len - sent))
              else do
                if len == 0 then
                    poke lenp 0 -- Entire
                  else
                    poke lenp (len - sent)
                hook
                threadWaitWrite dst
                let newhdr = remainingChunks (fromIntegral sent) hdr
                sendloopHeader dst src off lenp hook newhdr
          else
            throwErrno "Network.SendFile.MacOS.sendloopHeader"
  where
    hlen = fromIntegral . sum . map BS.length $ hdr
