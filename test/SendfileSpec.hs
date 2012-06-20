{-# LANGUAGE OverloadedStrings #-}

module SendfileSpec where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Conduit.Network
import Data.IORef
import Network.Sendfile
import Network.Socket
import System.Directory
import System.Exit
import System.IO
import System.Process
import Test.Hspec.ShouldBe
import System.Posix.Files
import System.Timeout

----------------------------------------------------------------

spec :: Spec
spec = do
    describe "sendfile" $ do
        it "sends an entire file" $ do
            sendFile EntireFile `shouldReturn` ExitSuccess
        it "sends a part of file" $ do
            sendFile (PartOfFile 2000 1000000) `shouldReturn` ExitSuccess
        it "does not fail in infinite loop even if the file is truncated" $ do
            timeout fiveSecs truncateFile `shouldReturn` Just ()
    describe "sendfileWithHeader" $ do
        it "sends an header and an entire file" $ do
            sendFileH EntireFile `shouldReturn` ExitSuccess
        it "sends an header and a part of file" $ do
            sendFileH (PartOfFile 2000 1000000) `shouldReturn` ExitSuccess
        it "does not fail in infinite loop even if the file is truncated" $ do
            timeout fiveSecs truncateFileH `shouldReturn` Just ()
  where
    fiveSecs = 5000000

----------------------------------------------------------------

sendFile :: FileRange -> IO ExitCode
sendFile range = sendFileCore range []

sendFileH :: FileRange -> IO ExitCode
sendFileH range = sendFileCore range headers
  where
    headers = [
        BS.replicate 100 'a'
      , BS.replicate 200 'b'
      , BS.replicate 300 'b'
      , "\n"
      ]

sendFileCore :: FileRange -> [ByteString] -> IO ExitCode
sendFileCore range headers = bracket setup teardown $ \(s2,_) -> do
    runResourceT $ sourceSocket s2 $$ sinkFile outputFile
    runResourceT $ copyfile range
    system $ "cmp -s " ++ outputFile ++ " " ++ expectedFile
  where
    copyfile EntireFile = do
        -- of course, we can use <> here
        sourceList headers $$ sinkFile expectedFile
        sourceFile inputFile $$ sinkAppendFile expectedFile
    copyfile (PartOfFile off len) = do
        sourceList headers $$ sinkFile expectedFile
        sourceFile inputFile $= CB.isolate (off' + len')
                             $$ (CB.take off' >> sinkAppendFile expectedFile)
      where
        off' = fromIntegral off
        len' = fromIntegral len
    setup = do
        (s1,s2) <- socketPair AF_UNIX Stream 0
        tid <- forkIO (sf s1 `finally` sendEOF s1)
        return (s2,tid)
      where
        sf s1
          | headers == [] = sendfile s1 inputFile range (return ())
          | otherwise     = sendfileWithHeader s1 inputFile range (return ()) headers
        sendEOF = sClose
    teardown (s2,tid) = do
        sClose s2
        killThread tid
        removeFileIfExists outputFile
        removeFileIfExists expectedFile
    inputFile = "test/inputFile"
    outputFile = "test/outputFile"
    expectedFile = "test/expectedFile"

----------------------------------------------------------------

truncateFile :: IO ()
truncateFile = truncateFileCore []

truncateFileH :: IO ()
truncateFileH = truncateFileCore headers
  where
    headers = [
        BS.replicate 100 'a'
      , BS.replicate 200 'b'
      , BS.replicate 300 'b'
      , "\n"
      ]

truncateFileCore :: [ByteString] -> IO ()
truncateFileCore headers = bracket setup teardown $ \(s2,_) -> do
    runResourceT $ sourceSocket s2 $$ sinkFile outputFile
    return ()
  where
    setup = do
        runResourceT $ sourceFile inputFile $$ sinkFile tempFile
        (s1,s2) <- socketPair AF_UNIX Stream 0
        ref <- newIORef (1 :: Int)
        tid <- forkIO (sf s1 ref `finally` sendEOF s1)
        return (s2,tid)
      where
        sf s1 ref
          | headers == [] = sendfile s1 tempFile range (hook ref)
          | otherwise     = sendfileWithHeader s1 tempFile range (hook ref) headers
        sendEOF = sClose
        hook ref = do
            n <- readIORef ref
            when (n == 10) $ setFileSize tempFile 900000
            writeIORef ref (n+1)
    teardown (s2,tid) = do
        sClose s2
        killThread tid
        removeFileIfExists tempFile
        removeFileIfExists outputFile
    inputFile = "test/inputFile"
    tempFile = "test/tempFile"
    outputFile = "test/outputFile"
    range = EntireFile


----------------------------------------------------------------

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = do
    exist <- doesFileExist file
    when exist $ removeFile file

sinkAppendFile :: MonadResource m
                  => FilePath
                  -> Sink ByteString m ()
sinkAppendFile fp = sinkIOHandle (openBinaryFile fp AppendMode)
