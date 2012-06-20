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
import Network.Sendfile
import Network.Socket
import System.Directory
import System.Exit
import System.IO
import System.Process
import Test.Hspec.ShouldBe

spec :: Spec
spec = do
    describe "sendfile" $ do
        it "sends an entire file" $ do
            runSendfile EntireFile `shouldReturn` ExitSuccess
        it "sends a part of file" $ do
            runSendfile (PartOfFile 2000 1000000) `shouldReturn` ExitSuccess
    describe "sendfileWithHeader" $ do
        it "sends an header and an entire file" $ do
            runSendfileH EntireFile `shouldReturn` ExitSuccess
        it "sends an header and a part of file" $ do
            runSendfileH (PartOfFile 2000 1000000) `shouldReturn` ExitSuccess

runSendfile :: FileRange -> IO ExitCode
runSendfile range = runSendfileCore range []

runSendfileH :: FileRange -> IO ExitCode
runSendfileH range = runSendfileCore range headers
  where
    headers = [
        BS.replicate 100 'a'
      , BS.replicate 200 'b'
      , BS.replicate 300 'b'
      , "\n"
      ]

runSendfileCore :: FileRange -> [ByteString] -> IO ExitCode
runSendfileCore range headers = bracket setup teardown $ \(s2,_) -> do
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

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = do
    exist <- doesFileExist file
    when exist $ removeFile file

sinkAppendFile :: MonadResource m
                  => FilePath
                  -> Sink ByteString m ()
sinkAppendFile fp = sinkIOHandle (openBinaryFile fp AppendMode)
