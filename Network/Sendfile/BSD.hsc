{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Sendfile.BSD (
    sendfile
  , sendfileFd
  , sendfileWithHeader
  , sendfileFdWithHeader
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

#include "HsConfig.h"
#include <sys/types.h>

entire :: COff
entire = 0

-- |
-- Simple binding for sendfile() of BSD and MacOS.
--
-- - Used system calls: open(), sendfile(), and close().
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfile :: Socket -> FilePath -> FileRange -> IO () -> IO ()
sendfile sock path range hook = bracket setup teardown $ \fd ->
    sendfileFd sock fd range hook
  where
    setup = openFd path ReadOnly Nothing defaultFileFlags
    teardown = closeFd

-- |
-- Simple binding for sendfile() of BSD and MacOS.
--
-- - Used system calls: sendfile()
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfileFd :: Socket -> Fd -> FileRange -> IO () -> IO ()
sendfileFd sock fd range hook =
    alloca $ \sentp -> do
        let (off,len) = case range of
                EntireFile           -> (0, entire)
                PartOfFile off' len' -> (fromInteger off', fromInteger len')
        sendloop dst fd off len sentp hook
  where
    dst = Fd $ fdSocket sock

sendloop :: Fd -> Fd -> COff -> COff -> Ptr COff -> IO () -> IO ()
sendloop dst src off len sentp hook = do
    rc <- sendFile src dst off len sentp nullPtr
    when (rc /= 0) $ do
        errno <- getErrno
        if errno `elem` [eAGAIN, eINTR] then do
            sent <- peek sentp
            hook
            -- Parallel IO manager use edge-trigger mode.
            -- So, calling threadWaitWrite only when errnor is eAGAIN.
            when (errno == eAGAIN) $ threadWaitWrite dst
            let newoff = off + sent
                newlen = if len == entire then entire else len - sent
            sendloop dst src newoff newlen sentp hook
          else
            throwErrno "Network.SendFile.MacOS.sendloop"

----------------------------------------------------------------

-- |
-- Simple binding for sendfile() of BSD and MacOS.
--
-- - Used system calls: open(), sendfile(), and close().
--
-- The fifth header is also sent with sendfile(). If the file is
-- small enough, the header and the file is send in a single TCP packet
-- on FreeBSD. MacOS sends the header and the file separately but it is
-- fast.
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfileWithHeader :: Socket -> FilePath -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileWithHeader sock path range hook hdr =
    bracket setup teardown $ \fd -> sendfileFdWithHeader sock fd range hook hdr
  where
    setup = openFd path ReadOnly Nothing defaultFileFlags
    teardown = closeFd

-- |
-- Simple binding for sendfile() of BSD and MacOS.
--
-- - Used system calls: sendfile()
--
-- The fifth header is also sent with sendfile(). If the file is
-- small enough, the header and the file is send in a single TCP packet
-- on FreeBSD. MacOS sends the header and the file separately but it is
-- fast.
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfileFdWithHeader :: Socket -> Fd -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileFdWithHeader sock fd range hook hdr =
    alloca $ \sentp ->
        if isFreeBSD && hlen >= 8192 then do
            -- If the length of the header is larger than 8191,
            -- threadWaitWrite does not come back on FreeBSD, sigh.
            -- We use writev() for the header and sendfile() for the file.
            sendMany sock hdr
            sendfileFd sock fd range hook
          else do
            -- On MacOS, the header and the body are sent separately.
            -- But it's fast. the writev() and sendfile() combination
            -- is also fast.
            let (off,len) = case range of
                    EntireFile           -> (0,entire)
                    PartOfFile off' len' -> (fromInteger off'
                                            ,fromInteger len' + hlen)
            mrc <- sendloopHeader dst fd off len sentp hdr hlen
            case mrc of
                Nothing              -> return ()
                Just (newoff,newlen) -> do
                    threadWaitWrite dst
                    sendloop dst fd newoff newlen sentp hook
  where
    dst = Fd $ fdSocket sock
    hlen = fromIntegral . sum . map BS.length $ hdr

sendloopHeader :: Fd -> Fd -> COff -> COff -> Ptr COff -> [ByteString] -> COff -> IO (Maybe (COff, COff))
sendloopHeader dst src off len sentp hdr hlen = do
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
                -- Parallel IO manager use edge-trigger mode.
                -- So, calling threadWaitWrite only when errnor is eAGAIN.
                when (errno == eAGAIN) $ threadWaitWrite dst
                let newlen = if len == entire then entire else len - sent
                    newhdr = remainingChunks (fromIntegral sent) hdr
                    newhlen = hlen - sent
                sendloopHeader dst src off newlen sentp newhdr newhlen
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
