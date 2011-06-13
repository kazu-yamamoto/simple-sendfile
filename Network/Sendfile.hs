{-# LANGUAGE CPP #-}

module Network.Sendfile (sendfile, FileRange(..)) where

import Network.Sendfile.Types

#ifdef OS_BSD
import Network.Sendfile.BSD
#elif  OS_MacOS
import Network.Sendfile.MacOS
#elif  OS_Linux
import Network.Sendfile.Linux
#elif  OS_Windows
import Network.Sendfile.Windows
#else
import Network.Sendfile.Fallback
#endif
