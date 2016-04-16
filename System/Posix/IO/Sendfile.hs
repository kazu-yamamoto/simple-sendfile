module System.Posix.IO.Sendfile (
    sendfile
  , I.sendfileFd
  , FileRange(..)
  ) where

import Control.Exception
import System.Posix.IO
import qualified System.Posix.IO.Sendfile.Internal as I
import System.Posix.IO.Sendfile.Types
import System.Posix.Types



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
-- This is meant to copy files. If you want to operate on sockets,
-- use `Network.Sendfile` instead.
--
-- This function can fail with EINVAL or ENOSYS on older linux
-- kernels, so you may want to catch these errors
-- and fall back to a different copy method.
sendfile :: Fd -> FilePath -> FileRange -> IO ()
sendfile out_fd path range = bracket setup teardown $ \fd ->
    I.sendfileFd out_fd fd range
  where
    setup = openFd path ReadOnly Nothing defaultFileFlags
    teardown = closeFd

----------------------------------------------------------------

