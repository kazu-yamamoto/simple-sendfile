{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Sendfile.Linux (sendfile) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Int (Int64)
import Data.Word
import Foreign.C.Error (throwErrno)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Network.Sendfile.Types
import Network.Socket
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types (Fd(..))

{-|
   Simple binding for sendfile() of MacOS.
   Used system calls:
     EntireFile -- open(), stat(), sendfile(), and close()
     PartOfFile -- open(), sendfile(), and close()
   If the size of the file is unknown when sending the entire file,
   specifying PartOfFile is much faster.
-}
sendfile :: Socket -> FilePath -> FileRange -> IO ()
sendfile sock path range = bracket
    (openFd path ReadOnly Nothing defaultFileFlags)
    closeFd
    sendfile'
  where
    dst = Fd $ fdSocket sock
    sendfile' fd = alloca $ \offp -> do
        case range of
            EntireFile -> do
                poke offp 0
                -- System call is very slow. Use PartOfFile instead.
                len <- fileSize <$> getFdStatus fd
                let len' = fromIntegral len
                sendPart dst fd offp len'
            PartOfFile off len -> do
                poke offp (fromIntegral off)
                let len' = fromIntegral len
                sendPart dst fd offp len'

sendPart :: Fd -> Fd -> Ptr (#type off_t) -> (#type size_t) -> IO ()
sendPart dst src offp len = do
    do bytes <- c_sendfile dst src offp len
       if bytes == -1
          then throwErrno "Network.SendFile.Linux.sendPart"
          else do
              let left = len - fromIntegral bytes
              when (left /= 0) $ do
                  threadWaitWrite dst
                  sendPart dst src offp left

-- Dst Src in order. take care
foreign import ccall unsafe "sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr (#type off_t) -> (#type size_t) -> IO (#type ssize_t)
