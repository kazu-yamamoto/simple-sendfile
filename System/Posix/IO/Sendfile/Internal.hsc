{-# LANGUAGE ForeignFunctionInterface #-}

module System.Posix.IO.Sendfile.Internal where

import Control.Applicative
import Control.Monad
import Foreign.C.Error (throwErrno)
import Foreign.C.Types
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke, sizeOf)
import System.Posix.Files.ByteString
import System.Posix.IO.Sendfile.Types
import System.Posix.Types


#include <sys/sendfile.h>
#include <sys/socket.h>

isLargeOffset :: Bool
isLargeOffset = sizeOf (0 :: COff) == 8

isLargeSize :: Bool
isLargeSize = sizeOf (0 :: CSize) == 8

safeSize :: CSize
safeSize
  | isLargeSize = 2^(60 :: Int)
  | otherwise   = 2^(30 :: Int)



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
sendfileFd :: Fd -> Fd -> FileRange -> IO ()
sendfileFd dst fd range =
    alloca $ \offp -> case range of
        EntireFile -> do
            poke offp 0
            -- System call is very slow. Use PartOfFile instead.
            len <- fileSize <$> getFdStatus fd
            let len' = fromIntegral len
            sendfileloop dst fd offp len'
        PartOfFile off len -> do
            poke offp (fromIntegral off)
            let len' = fromIntegral len
            sendfileloop dst fd offp len'


sendfileloop :: Fd -> Fd -> Ptr COff -> CSize -> IO ()
sendfileloop dst src offp len = do
    -- Multicore IO manager use edge-trigger mode.
    -- So, calling threadWaitWrite only when errnor is eAGAIN.
    let toSend
          | len > safeSize = safeSize
          | otherwise      = len
    bytes <- c_sendfile dst src offp toSend
    case bytes of
        -1 -> throwErrno "System.Posix.IO.Sendfile.Internal"
        0  -> return () -- the file is truncated
        _  -> do
            let left = len - fromIntegral bytes
            when (left /= 0) $ sendfileloop dst src offp left


-- Dst Src in order. take care
foreign import ccall unsafe "sendfile"
    c_sendfile32 :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize

foreign import ccall unsafe "sendfile64"
    c_sendfile64 :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize

c_sendfile :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize
c_sendfile
  | isLargeOffset = c_sendfile64
  | otherwise     = c_sendfile32
