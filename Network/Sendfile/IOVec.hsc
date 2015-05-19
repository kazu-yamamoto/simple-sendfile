{- Original: Network/Socket/ByteString/* -}

-- | Support module for the POSIX writev system call.
module Network.Sendfile.IOVec (
    IOVec(..)
  , SfHdtr(..)
  , withSfHdtr
  , remainingChunks
  ) where

import Control.Monad (zipWithM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Marshal (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (Storable(..))

#include "HsConfig.h"
#include <sys/uio.h>
#include <sys/socket.h>

----------------------------------------------------------------

data IOVec = IOVec {
    iovBase :: Ptr CChar
  , iovLen  :: CSize
  }

instance Storable IOVec where
  sizeOf _    = (#const sizeof(struct iovec))
  alignment _ = alignment (undefined :: CInt)

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
  sizeOf _    = (#const sizeof(struct sf_hdtr))
  alignment _ = alignment (undefined :: CInt)

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

withIOVec :: [ByteString] -> ((Ptr IOVec, Int) -> IO a) -> IO a
withIOVec cs f =
    allocaArray csLen $ \aPtr -> do
        zipWithM_ pokeIov (ptrs aPtr) cs
        f (aPtr, csLen)
  where
    csLen = length cs
    ptrs = iterate (`plusPtr` sizeOf (undefined :: IOVec))
    pokeIov ptr s =
        unsafeUseAsCStringLen s $ \(sPtr, sLen) ->
        poke ptr $ IOVec sPtr (fromIntegral sLen)

withSfHdtr :: [ByteString] -> (Ptr SfHdtr -> IO a) -> IO a
withSfHdtr cs f = withIOVec cs $ \(iovecp,len) ->
    alloca $ \sfhdtrp -> do
        poke sfhdtrp $ SfHdtr iovecp (fromIntegral len)
        f sfhdtrp

----------------------------------------------------------------

remainingChunks :: Int -> [ByteString] -> [ByteString]
remainingChunks _ [] = []
remainingChunks i (x:xs)
    | i < len        = BS.drop i x : xs
    | otherwise      = let i' = i - len in i' `seq` remainingChunks i' xs
  where
    len = BS.length x
