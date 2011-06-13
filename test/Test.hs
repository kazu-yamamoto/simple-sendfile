module Main where

import Control.Concurrent
import Network.Sendfile
import Network.Socket
import System.Environment
import System.IO

main :: IO ()
main = do
    [file] <- getArgs
    (s1,s2) <- socketPair AF_UNIX Stream 0
    hSetEncoding stdout latin1
    forkIO $ saver s2
    sendfile s1 file EntireFile

saver :: Socket -> IO ()
saver s = do
    cs <- recv s 4096
    if cs == ""
       then return ()
       else do
        putStr cs
        saver s
