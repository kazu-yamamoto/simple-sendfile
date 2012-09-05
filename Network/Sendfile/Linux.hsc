{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Sendfile.Linux (
    sendfile
  , sendfileFd
  , sendfileWithHeader
  , sendfileFdWithHeader
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString as B
import Data.ByteString.Unsafe
import Data.Int
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import GHC.Conc (threadWaitWrite)
import Network.Sendfile.Types
import Network.Socket
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

#include <sys/sendfile.h>
#include <sys/socket.h>

----------------------------------------------------------------

-- |
-- Simple binding for sendfile() of Linux.
-- Used system calls:
--
--  - EntireFile -- open(), stat(), sendfile(), and close()
--
--  - PartOfFile -- open(), sendfile(), and close()
--
-- If the size of the file is unknown when sending the entire file,
-- specifying PartOfFile is much faster.
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
-- Simple binding for sendfile() of Linux.
-- Used system calls:
--
--  - EntireFile -- stat() and sendfile()
--
--  - PartOfFile -- sendfile()
--
-- If the size of the file is unknown when sending the entire file,
-- specifying PartOfFile is much faster.
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfileFd :: Socket -> Fd -> FileRange -> IO () -> IO ()
sendfileFd sock fd range hook =
    alloca $ \offp -> case range of
        EntireFile -> do
            poke offp 0
            -- System call is very slow. Use PartOfFile instead.
            len <- fileSize <$> getFdStatus fd
            let len' = fromIntegral len
            sendloop dst fd offp len' hook
        PartOfFile off len -> do
            poke offp (fromIntegral off)
            let len' = fromIntegral len
            sendloop dst fd offp len' hook
  where
    dst = Fd $ fdSocket sock

sendloop :: Fd -> Fd -> Ptr COff -> CSize -> IO () -> IO ()
sendloop dst src offp len hook = do
    bytes <- c_sendfile dst src offp len
    case bytes of
        -1 -> do
            errno <- getErrno
            if errno == eAGAIN then
                loop len
              else
                throwErrno "Network.SendFile.Linux.sendloop"
        0  -> return () -- the file is truncated
        _  -> loop (len - fromIntegral bytes)
  where
    loop 0    = return ()
    loop left = do
        hook
        threadWaitWrite dst
        sendloop dst src offp left hook

-- Dst Src in order. take care
foreign import ccall unsafe "sendfile"
    c_sendfile :: Fd -> Fd -> Ptr COff -> CSize -> IO (#type ssize_t)

----------------------------------------------------------------

-- |
-- Simple binding for send() and sendfile() of Linux.
-- Used system calls:
--
--  - EntireFile -- send(), open(), stat(), sendfile(), and close()
--
--  - PartOfFile -- send(), open(), sendfile(), and close()
--
-- The fifth header is sent with send() + the MSG_MORE flag. If the
-- file is small enough, the header and the file is send in a single
-- TCP packet.
--
-- If the size of the file is unknown when sending the entire file,
-- specifying PartOfFile is much faster.
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfileWithHeader :: Socket -> FilePath -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileWithHeader sock path range hook hdr = do
    -- Copying is much faster than syscall.
    sendAllMsgMore sock $ B.concat hdr
    sendfile sock path range hook

-- |
-- Simple binding for send() and sendfile() of Linux.
-- Used system calls:
--
--  - EntireFile -- send(), stat() and sendfile()
--
--  - PartOfFile -- send() and sendfile()
--
-- The fifth header is sent with send() + the MSG_MORE flag. If the
-- file is small enough, the header and the file is send in a single
-- TCP packet.
--
-- If the size of the file is unknown when sending the entire file,
-- specifying PartOfFile is much faster.
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfileFdWithHeader :: Socket -> Fd -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileFdWithHeader sock fd range hook hdr = do
    -- Copying is much faster than syscall.
    sendAllMsgMore sock $ B.concat hdr
    sendfileFd sock fd range hook

sendAllMsgMore :: Socket -> ByteString -> IO ()
sendAllMsgMore sock bs = do
    sent <- sendMsgMore sock bs
    when (sent < B.length bs) $ sendAllMsgMore sock (B.drop sent bs)

sendMsgMore :: Socket -> ByteString -> IO Int
sendMsgMore (MkSocket s _ _ _ _) xs =
    unsafeUseAsCStringLen xs $ \(str, len) ->
    fromIntegral <$> throwSocketErrorIfMinus1RetryMayBlock
                         "sendMsgMore"
                         (threadWaitWrite (fromIntegral s))
                         (c_send s str (fromIntegral len) (#const MSG_MORE))

foreign import ccall unsafe "send"
  c_send :: CInt -> Ptr CChar -> CSize -> CInt -> IO (#type ssize_t)
