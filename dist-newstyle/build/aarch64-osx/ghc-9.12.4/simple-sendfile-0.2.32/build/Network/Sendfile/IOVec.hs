{-# LINE 1 "Network/Sendfile/IOVec.hsc" #-}
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




----------------------------------------------------------------

data IOVec = IOVec {
    iovBase :: Ptr CChar
  , iovLen  :: CSize
  }

instance Storable IOVec where
  sizeOf _    = (16)
{-# LINE 33 "Network/Sendfile/IOVec.hsc" #-}
  alignment _ = alignment (undefined :: CInt)

  peek p = do
      base <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 37 "Network/Sendfile/IOVec.hsc" #-}
      len  <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))  p
{-# LINE 38 "Network/Sendfile/IOVec.hsc" #-}
      return $ IOVec base len

  poke p iov = do
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (iovBase iov)
{-# LINE 42 "Network/Sendfile/IOVec.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8))  p (iovLen  iov)
{-# LINE 43 "Network/Sendfile/IOVec.hsc" #-}

----------------------------------------------------------------

data SfHdtr = SfHdtr {
    sfhdtrHdr    :: Ptr IOVec
  , sfhdtrHdrLen :: CInt
  }

instance Storable SfHdtr where
  sizeOf _    = (32)
{-# LINE 53 "Network/Sendfile/IOVec.hsc" #-}
  alignment _ = alignment (undefined :: CInt)

  peek p = do
      hdr  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 57 "Network/Sendfile/IOVec.hsc" #-}
      hlen <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))  p
{-# LINE 58 "Network/Sendfile/IOVec.hsc" #-}
      return $ SfHdtr hdr hlen

  poke p sfhdtr = do
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  p (sfhdtrHdr sfhdtr)
{-# LINE 62 "Network/Sendfile/IOVec.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8))  p (sfhdtrHdrLen sfhdtr)
{-# LINE 63 "Network/Sendfile/IOVec.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p nullPtr
{-# LINE 64 "Network/Sendfile/IOVec.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 24))  p (0 :: CInt)
{-# LINE 65 "Network/Sendfile/IOVec.hsc" #-}

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
