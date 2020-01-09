{-# LANGUAGE CPP #-}

{-|
  Cross platform library for the sendfile system call.
  This library tries to call minimum system calls which
  are the bottleneck of web servers.
-}

module Network.Sendfile (
    sendfile
  , sendfileWithHeader
  , sendfileFd
  , sendfileFdWithHeader
  , FileRange(..)
  ) where

import Network.Sendfile.Types

#ifdef OS_BSD
import Network.Sendfile.BSD
#elif  OS_MacOS
import Network.Sendfile.BSD
#elif  OS_Linux
import Network.Sendfile.Linux
#else
import Network.Sendfile.Fallback
#endif
