{-# LANGUAGE OverloadedStrings #-}

{-
  runghc -i.. -DOS_MacOS Test.hs input 100 1000 > output
  cmp input output -i 100:0 -n 1000
-}

module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import Network.Sendfile
import Network.Socket
import qualified Network.Socket.ByteString as SB
import System.Environment
import System.IO

main :: IO ()
main = do
    (file,range) <- parseArgs
    (s1,s2) <- socketPair AF_UNIX Stream 0
    hSetEncoding stdout latin1
    forkIO $ do
        sendfile s1 file range
        sClose s1
    saver s2

parseArgs :: IO (FilePath,FileRange)
parseArgs = do
    cs <- getArgs
    case cs of
        [file]         -> return (file, EntireFile)
        [file,off,len] -> return (file, PartOfFile (read off) (read len))
        _              -> error "file [off len]"

saver :: Socket -> IO ()
saver s = do
    bs <- SB.recv s 4096
    when (bs /= "") $ do
        BS.putStr bs
        saver s
