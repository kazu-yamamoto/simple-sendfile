{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Sendfile.MacOS (sendfile) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Int (Int64)
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types (CInt)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Network.Sendfile.Types
import Network.Socket
import System.Posix.IO
import System.Posix.Types (Fd(..))

import System.IO

sendfile :: Socket -> FilePath -> FileRange -> IO ()
sendfile sock path range = bracket
    (openFd path ReadOnly Nothing defaultFileFlags)
    closeFd
    (\fd -> alloca $ \lenp -> do
        case range of
            EntireFile -> do
                poke lenp 0
                sendLoop0 dst fd 0 lenp
            PartOfFile off len -> do
                let off' = fromInteger off
                poke lenp (fromInteger len)
                sendLoop dst fd off' lenp)
  where
    dst = Fd $ fdSocket sock

sendLoop0 :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO ()
sendLoop0 dst src off lenp = do
    do rc <- c_sendfile src dst off lenp
       when (rc /= 0) $ do
           errno <- getErrno
           if errno == eAGAIN || errno == eINTR
              then do
                  sent <- peek lenp
                  poke lenp 0
                  hPutStrLn stderr $ show off ++ " " ++ show sent
                  threadWaitWrite dst
                  sendLoop0 dst src (off + sent) lenp
              else throwErrno "Network.SendFile.MacOS.sendLoop0"

sendLoop :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO ()
sendLoop dst src off lenp = do
    do len <- peek lenp
       rc <- c_sendfile src dst off lenp
       when (rc /= 0) $ do
           errno <- getErrno
           if errno == eAGAIN || errno == eINTR
              then do
                  sent <- peek lenp
                  poke lenp (len - sent)
                  sendLoop dst src (off + sent) lenp
                  threadWaitWrite dst
              else throwErrno "Network.SendFile.MacOS.sendLoop"

c_sendfile :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO CInt
c_sendfile fd s offset lenp = c_sendfile' fd s offset lenp nullPtr 0

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile'
    :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> Ptr () -> CInt -> IO CInt

