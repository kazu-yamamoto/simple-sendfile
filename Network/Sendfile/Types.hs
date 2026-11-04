{-# LANGUAGE CPP #-}

module Network.Sendfile.Types where

import GHC.IO.Exception (IOErrorType(InvalidArgument), IOException(..))

#ifdef VERSION_unix
import qualified Data.ByteString as B

import qualified System.Posix.IO            as P
import qualified System.Posix.IO.ByteString as PB
import qualified System.Posix.Types         as P
#endif

-- |
--  File range for 'sendfile'.
--
--  * If the offset or the length of 'PartOfFile' is negative, an
--    'IOException' of 'InvalidArgument' is thrown before anything is sent.
--
--  * If the length is 0, nothing is sent (only the header for the
--    @WithHeader@ variants).
--
--  * If the file is shorter than the range, or is truncated while
--    being sent, sending stops at the end of the file without any
--    error. So, the number of bytes sent may be less than requested.

data FileRange = EntireFile
               | PartOfFile {
                   rangeOffset :: Integer
                 , rangeLength :: Integer
                 }

-- | Throws 'InvalidArgument' if the offset or the length is negative.
checkRange :: FileRange -> IO ()
checkRange (PartOfFile off len)
  | off < 0 || len < 0 = ioError $ IOError Nothing InvalidArgument
        "Network.Sendfile" "negative offset or length" Nothing Nothing
checkRange _ = return ()

-- To avoid build error in windows
#ifdef VERSION_unix

-- | 'openFd's signature changed in @unix-2.8@. This is a portable wrapper.
openFd :: FilePath -> P.OpenMode -> P.OpenFileFlags -> IO P.Fd
#if MIN_VERSION_unix(2,8,0)
openFd path mode flags = P.openFd path mode flags
#else
openFd path mode flags = P.openFd path mode Nothing flags
#endif

-- | 'openFd's signature changed in @unix-2.8@. This is a portable wrapper.
openFdBS :: B.ByteString -> P.OpenMode -> P.OpenFileFlags -> IO P.Fd
#if MIN_VERSION_unix(2,8,0)
openFdBS path mode flags = PB.openFd path mode flags
#else
openFdBS path mode flags = PB.openFd path mode Nothing flags
#endif

#endif
