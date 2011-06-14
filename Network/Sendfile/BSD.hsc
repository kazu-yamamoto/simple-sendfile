{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Sendfile.BSD (sendfile) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Network.Sendfile.Types
import Network.Socket
import System.Posix.IO
import System.Posix.Types (Fd(..), COff)

{-|
   Simple binding for sendfile() of MacOS.
   Used system calls: open(), sendfile(), and close().
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
            EntireFile -> sendEntire dst fd 0 lenp
            PartOfFile off len -> do
                let off' = fromInteger off
                    len' = fromInteger len
                sendPart dst fd off' len' lenp

sendEntire :: Fd -> Fd -> COff -> Ptr COff -> IO ()
sendEntire dst src off lenp = do
    do rc <- c_sendfile src dst off 0 lenp
       when (rc /= 0) $ do
           errno <- getErrno
           if errno == eAGAIN || errno == eINTR
              then do
                  sent <- peek lenp
                  threadWaitWrite dst
                  sendEntire dst src (off + sent) lenp
              else throwErrno "Network.SendFile.BSD.sendEntire"

sendPart :: Fd -> Fd -> COff -> CSize -> Ptr COff -> IO ()
sendPart dst src off len lenp = do
    do rc <- c_sendfile src dst off len lenp
       when (rc /= 0) $ do
           errno <- getErrno
           if errno == eAGAIN || errno == eINTR
              then do
                  sent <- peek lenp
                  threadWaitWrite dst
                  let off' = off + sent
                      len' = len - fromIntegral sent
                  sendPart dst src off' len' lenp
              else throwErrno "Network.SendFile.BSD.sendPart"

c_sendfile :: Fd -> Fd -> COff -> CSize -> Ptr COff -> IO CInt
c_sendfile fd s offset len lenp = c_sendfile' fd s offset len nullPtr lenp 0

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile'
    :: Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt -> IO CInt
