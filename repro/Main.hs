{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- Minimal repro: does recv() on one end of a socketPair return EOF
-- when the other end is closed (possibly without sending anything)?
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Maybe (isNothing)
import Network.Socket
import qualified Network.Socket.ByteString as SB
import System.Environment (getArgs, lookupEnv)
import System.Process (getCurrentPid, callProcess)
import System.IO
import System.Timeout (timeout)

drain :: Socket -> IO ()
drain s = do
    bs <- SB.recv s 4096
    unless (B.null bs) $ drain s

-- Returns True if the reader hung (no EOF within 2 seconds).
oneIter :: String -> IO Bool
oneIter mode = do
    (s1, s2) <- socketPair AF_UNIX Stream 0
    r <- case mode of
        "closefirst" -> close s1 >> timeout 2000000 (drain s2)
        _ -> do
            let sender = case mode of
                    "nosend"       -> close s1
                    "nosend-delay" -> threadDelay 1000 >> close s1
                    "sendone"      -> SB.sendAll s1 "x" >> close s1
                    _              -> error ("unknown mode: " ++ mode)
            _ <- forkIO sender
            timeout 2000000 (drain s2)
    when (isNothing r) $ do
        -- Snapshot all thread stacks before touching s2 (CDB = path to cdb.exe).
        mcdb <- lookupEnv "CDB"
        forM_ mcdb $ \cdb -> do
            pid <- getCurrentPid
            putStrLn ("  attaching cdb to pid " ++ show pid)
            callProcess cdb ["-p", show pid, "-c", "~*k 30; qd"]
                `catch` \(e :: SomeException) -> putStrLn ("  cdb failed: " ++ show e)
        -- Probe: is EOF already visible at the OS level?
        r2 <- timeout 1000000 (SB.recv s2 4096)
        putStrLn ("  hang (" ++ mode ++ "): second recv = " ++ show r2)
    close s2
    return (isNothing r)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [label, n] <- getArgs
    forM_ ["nosend", "nosend-delay", "sendone", "closefirst"] $ \mode -> do
        let loop i hangs
              | i > read n || hangs >= 5 = return (i - 1, hangs)
              | otherwise = do
                  h <- oneIter mode `catch` \(e :: SomeException) -> do
                      putStrLn ("  exception: " ++ show e)
                      return False
                  loop (i + 1) (if h then hangs + 1 else hangs)
        (done, hangs) <- loop (1 :: Int) (0 :: Int)
        putStrLn ("RESULT " ++ label ++ " " ++ mode ++ ": "
                  ++ show hangs ++ " hangs / " ++ show done ++ " iterations")
