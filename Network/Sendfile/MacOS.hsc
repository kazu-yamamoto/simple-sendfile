{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Sendfile.MacOS (sendfile) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Int
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types (CInt)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Network.Sendfile.Types
import Network.Socket
import System.Posix.IO
import System.Posix.Types (Fd(..))

{-|
   Simple binding for sendfile() of MacOS.

   - Used system calls: open(), sendfile(), and close().
-}
sendfile :: Socket -> FilePath -> FileRange -> IO ()
sendfile sock path range = bracket
    (openFd path ReadOnly Nothing defaultFileFlags)
    closeFd
    sendfile'
  where
    dst = Fd $ fdSocket sock
    sendfile' fd = alloca $ \lenp -> do
        case range of
            EntireFile -> do
                poke lenp 0
                sendEntire dst fd 0 lenp
            PartOfFile off len -> do
                let off' = fromInteger off
                poke lenp (fromInteger len)
                sendPart dst fd off' lenp

sendEntire :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO ()
sendEntire dst src off lenp = do
    do rc <- c_sendfile src dst off lenp
       when (rc /= 0) $ do
           errno <- getErrno
           if errno == eAGAIN || errno == eINTR
              then do
                  sent <- peek lenp
                  poke lenp 0
                  threadWaitWrite dst
                  sendEntire dst src (off + sent) lenp
              else throwErrno "Network.SendFile.MacOS.sendEntire"

sendPart :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO ()
sendPart dst src off lenp = do
    do len <- peek lenp
       rc <- c_sendfile src dst off lenp
       when (rc /= 0) $ do
           errno <- getErrno
           if errno == eAGAIN || errno == eINTR
              then do
                  sent <- peek lenp
                  poke lenp (len - sent)
                  threadWaitWrite dst
                  sendPart dst src (off + sent) lenp
              else throwErrno "Network.SendFile.MacOS.sendPart"

c_sendfile :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO CInt
c_sendfile fd s offset lenp = c_sendfile' fd s offset lenp nullPtr 0

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile'
    :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> Ptr () -> CInt -> IO CInt
