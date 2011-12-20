{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Network.Sendfile.MacOS (sendfile) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Int
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CInt(CInt))
#else
import Foreign.C.Types (CInt)
#endif
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Network.Sendfile.Types
import Network.Socket
import System.Posix.IO
import System.Posix.Types (Fd(..))

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
sendfile sock path range hook = bracket
    (openFd path ReadOnly Nothing defaultFileFlags)
    closeFd
    sendfile'
  where
    dst = Fd $ fdSocket sock
    sendfile' fd = alloca $ \lenp -> do
        case range of
            EntireFile -> do
                poke lenp 0
                sendEntire dst fd 0 lenp hook
            PartOfFile off len -> do
                let off' = fromInteger off
                poke lenp (fromInteger len)
                sendPart dst fd off' lenp hook

sendEntire :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO () -> IO ()
sendEntire dst src off lenp hook = do
    do rc <- c_sendfile src dst off lenp
       when (rc /= 0) $ do
           errno <- getErrno
           if errno `elem` [eAGAIN, eINTR]
              then do
                  sent <- peek lenp
                  poke lenp 0
                  hook
                  threadWaitWrite dst
                  sendEntire dst src (off + sent) lenp hook
              else throwErrno "Network.SendFile.MacOS.sendEntire"

sendPart :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO () -> IO ()
sendPart dst src off lenp hook = do
    do len <- peek lenp
       rc <- c_sendfile src dst off lenp
       when (rc /= 0) $ do
           errno <- getErrno
           if errno `elem` [eAGAIN, eINTR]
              then do
                  sent <- peek lenp
                  poke lenp (len - sent)
                  hook
                  threadWaitWrite dst
                  sendPart dst src (off + sent) lenp hook
              else throwErrno "Network.SendFile.MacOS.sendPart"

c_sendfile :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO CInt
c_sendfile fd s offset lenp = c_sendfile' fd s offset lenp nullPtr 0

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile'
    :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> Ptr () -> CInt -> IO CInt
