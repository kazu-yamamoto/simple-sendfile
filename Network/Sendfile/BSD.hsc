{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Sendfile.BSD (
    sendfile
  , sendfileWithHeader
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Network.Sendfile.IOVec
import Network.Sendfile.Types
import Network.Socket
import Network.Socket.ByteString
import System.Posix.IO
import System.Posix.Types

#include <sys/types.h>

entire :: COff
entire = 0

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
    alloca $ \sentp -> do
        let (off,len) = case range of
                EntireFile           -> (0, entire)
                PartOfFile off' len' -> (fromInteger off', fromInteger len')
        sendloop dst fd off len sentp hook
  where
    setup = openFd path ReadOnly Nothing defaultFileFlags
    teardown = closeFd
    dst = Fd $ fdSocket sock

sendloop :: Fd -> Fd -> COff -> COff -> Ptr COff -> IO () -> IO ()
sendloop dst src off len sentp hook = do
    rc <- sendFile src dst off len sentp nullPtr
    when (rc /= 0) $ do
        errno <- getErrno
        if errno `elem` [eAGAIN, eINTR] then do
            sent <- peek sentp
            hook
            threadWaitWrite dst
            let newoff = off + sent
                newlen = if len == entire then entire else len - sent
            sendloop dst src newoff newlen sentp hook
          else
            throwErrno "Network.SendFile.MacOS.sendloop"

----------------------------------------------------------------

sendfileWithHeader :: Socket -> FilePath -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileWithHeader sock path range hook hdr =
    bracket setup teardown $ \fd -> alloca $ \sentp ->
        if isFreeBSD && hlen >= 8192 then do
            sendMany sock hdr
            hook
            sendfile sock path range hook
          else do
            let (off,len) = case range of
                    EntireFile           -> (0,entire)
                    PartOfFile off' len' -> (fromInteger off'
                                            ,fromInteger len' + hlen)
            mrc <- sendloopHeader dst fd off len sentp hook hdr hlen
            case mrc of
                Nothing              -> return ()
                Just (newoff,newlen) -> do
                    hook
                    threadWaitWrite dst
                    sendloop dst fd newoff newlen sentp hook
  where
    setup = openFd path ReadOnly Nothing defaultFileFlags
    teardown = closeFd
    dst = Fd $ fdSocket sock
    hlen = fromIntegral . sum . map BS.length $ hdr

sendloopHeader :: Fd -> Fd -> COff -> COff -> Ptr COff -> IO () -> [ByteString] -> COff -> IO (Maybe (COff, COff))
sendloopHeader dst src off len sentp hook hdr hlen = do
    rc <- withSfHdtr hdr $ sendFile src dst off len sentp
    if rc == 0 then
        return Nothing
      else do
        errno <- getErrno
        if errno `elem` [eAGAIN, eINTR] then do
            sent <- peek sentp
            if sent >= hlen then do
                let newoff = off + sent - hlen
                if len == entire then
                    return $ Just (newoff, entire)
                  else
                    return $ Just (newoff, len - sent)
              else do
                hook
                threadWaitWrite dst
                let newlen = if len == entire then entire else len - sent
                    newhdr = remainingChunks (fromIntegral sent) hdr
                    newhlen = hlen - sent
                sendloopHeader dst src off newlen sentp hook newhdr newhlen
          else
            throwErrno "Network.SendFile.MacOS.sendloopHeader"

----------------------------------------------------------------

#ifdef OS_MacOS
-- Shuffle the order of arguments for currying.
sendFile :: Fd -> Fd -> COff -> COff -> Ptr COff -> Ptr SfHdtr -> IO CInt
sendFile fd s off len sentp hdrp = do
    poke sentp len
    c_sendfile fd s off sentp hdrp 0

foreign import ccall unsafe "sys/uio.h sendfile"
    c_sendfile :: Fd -> Fd -> COff -> Ptr COff -> Ptr SfHdtr -> CInt -> IO CInt

isFreeBSD :: Bool
isFreeBSD = False
#else
-- Let's don't use CSize for 'len' and use COff for convenience.
-- Shuffle the order of arguments for currying.
sendFile :: Fd -> Fd -> COff -> COff -> Ptr COff -> Ptr SfHdtr -> IO CInt
sendFile fd s off len sentp hdrp =
    c_sendfile fd s off (fromIntegral len) hdrp sentp 0

foreign import ccall unsafe "sys/uio.h sendfile"
    c_sendfile :: Fd -> Fd -> COff -> CSize -> Ptr SfHdtr -> Ptr COff -> CInt -> IO CInt

isFreeBSD :: Bool
isFreeBSD = True
#endif
