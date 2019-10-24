{-# LANGUAGE CPP #-}

module Network.Sendfile.Types where

#ifdef VERSION_unix
import qualified Data.ByteString as B

import qualified System.Posix.IO            as P
import qualified System.Posix.IO.ByteString as PB
import qualified System.Posix.Types         as P
#endif

-- |
--  File range for 'sendfile'.

data FileRange = EntireFile
               | PartOfFile {
                   rangeOffset :: Integer
                 , rangeLength :: Integer
                 }

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
