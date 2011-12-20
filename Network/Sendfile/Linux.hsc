{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Sendfile.Linux (sendfile) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.Int
import Data.Word
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Network.Sendfile.Types
import Network.Socket
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types (Fd(..))

#include <sys/sendfile.h>

{-|
   Simple binding for sendfile() of Linux.
   Used system calls:

     - EntireFile -- open(), stat(), sendfile(), and close()
     - PartOfFile -- open(), sendfile(), and close()

   If the size of the file is unknown when sending the entire file,
   specifying PartOfFile is much faster.

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
    sendfile' fd = alloca $ \offp -> do
        case range of
            EntireFile -> do
                poke offp 0
                -- System call is very slow. Use PartOfFile instead.
                len <- fileSize <$> getFdStatus fd
                let len' = fromIntegral len
                sendPart dst fd offp len' hook
            PartOfFile off len -> do
                poke offp (fromIntegral off)
                let len' = fromIntegral len
                sendPart dst fd offp len' hook

sendPart :: Fd -> Fd -> Ptr (#type off_t) -> (#type size_t) -> IO () -> IO ()
sendPart dst src offp len hook = do
    do bytes <- c_sendfile dst src offp len
       case bytes of
           -1 -> do errno <- getErrno
                    if errno == eAGAIN
                       then loop len
                       else throwErrno "Network.SendFile.Linux.sendPart"
           0  -> return () -- the file is truncated
           _  -> loop (len - fromIntegral bytes)
  where
    loop left
      | left == 0 = return ()
      | otherwise = do
          hook
          threadWaitWrite dst
          sendPart dst src offp left hook

-- Dst Src in order. take care
foreign import ccall unsafe "sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr (#type off_t) -> (#type size_t) -> IO (#type ssize_t)
