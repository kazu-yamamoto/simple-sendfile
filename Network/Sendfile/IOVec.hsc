{- Original: Network/Socket/ByteString/* -}

-- | Support module for the POSIX writev system call.
module Network.Sendfile.IOVec (
    IOVec(..)
  , SfHdtr(..)
  , withSfHdtr
  , remainingChunks
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Marshal (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (Storable(..))

#include <limits.h>
#include <sys/uio.h>
#include <sys/socket.h>

----------------------------------------------------------------

data IOVec = IOVec {
    iovBase :: Ptr CChar
  , iovLen  :: CSize
  }

instance Storable IOVec where
  sizeOf _    = (#size struct iovec)
  alignment _ = (#alignment struct iovec)

  peek p = do
      base <- (#peek struct iovec, iov_base) p
      len  <- (#peek struct iovec, iov_len)  p
      return $ IOVec base len

  poke p iov = do
      (#poke struct iovec, iov_base) p (iovBase iov)
      (#poke struct iovec, iov_len)  p (iovLen  iov)

----------------------------------------------------------------

data SfHdtr = SfHdtr {
    sfhdtrHdr    :: Ptr IOVec
  , sfhdtrHdrLen :: CInt
  }

instance Storable SfHdtr where
  sizeOf _    = (#size struct sf_hdtr)
  alignment _ = (#alignment struct sf_hdtr)

  peek p = do
      hdr  <- (#peek struct sf_hdtr, headers) p
      hlen <- (#peek struct sf_hdtr, hdr_cnt)  p
      return $ SfHdtr hdr hlen

  poke p sfhdtr = do
      (#poke struct sf_hdtr, headers)  p (sfhdtrHdr sfhdtr)
      (#poke struct sf_hdtr, hdr_cnt)  p (sfhdtrHdrLen sfhdtr)
      (#poke struct sf_hdtr, trailers) p nullPtr
      (#poke struct sf_hdtr, trl_cnt)  p (0 :: CInt)

----------------------------------------------------------------

-- The buffers stay valid while 'f' runs since the calls are nested.
withIOVec :: [ByteString] -> ((Ptr IOVec, Int) -> IO a) -> IO a
withIOVec cs f =
    allocaArray csLen $ \aPtr -> go aPtr aPtr cs
  where
    csLen = length cs
    go aPtr _ [] = f (aPtr, csLen)
    go aPtr ptr (s:ss) =
        unsafeUseAsCStringLen s $ \(sPtr, sLen) -> do
            poke ptr $ IOVec sPtr (fromIntegral sLen)
            go aPtr (ptr `plusPtr` sizeOf (undefined :: IOVec)) ss

-- Too many chunks cause EINVAL, so they are concatenated.
withSfHdtr :: [ByteString] -> (Ptr SfHdtr -> IO a) -> IO a
withSfHdtr cs0 f = withIOVec cs $ \(iovecp,len) ->
    alloca $ \sfhdtrp -> do
        poke sfhdtrp $ SfHdtr iovecp (fromIntegral len)
        f sfhdtrp
  where
    cs | length cs0 > (#const IOV_MAX) = [BS.concat cs0]
       | otherwise                     = cs0

----------------------------------------------------------------

remainingChunks :: Int -> [ByteString] -> [ByteString]
remainingChunks _ [] = []
remainingChunks i (x:xs)
    | i < len        = BS.drop i x : xs
    | otherwise      = let i' = i - len in i' `seq` remainingChunks i' xs
  where
    len = BS.length x
