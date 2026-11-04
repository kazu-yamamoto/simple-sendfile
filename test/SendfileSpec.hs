{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SendfileSpec where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Conduit.Network
import Data.IORef
import Network.Sendfile
import Network.Socket
import System.Directory
import System.IO
import System.Timeout
import System.EasyFile
import Test.Hspec
import System.IO.Unsafe (unsafePerformIO)

----------------------------------------------------------------

spec :: Spec
spec = do
    describe "sendfile" $ do
        it "sends an entire file" $ do
            sendFile EntireFile `shouldReturn` True
        it "sends a part of file" $ do
            sendFile (PartOfFile 2000 1000000) `shouldReturn` True
        it "sends nothing for a zero-length part" $ do
            sendFile (PartOfFile 2000 0) `shouldReturn` True
        it "terminates even if length is over" $ do
            shouldTerminate $ sendIllegal (PartOfFile 2000 5000000)
        it "terminates even if offset is over" $ do
            shouldTerminate $ sendIllegal (PartOfFile 5000000 6000000)
        -- On Windows, setFileSize throws an exception due to the
        -- access permission. The test case will be finished but it is
        -- not a right test.
        it "terminates even if the file is truncated" $ do
            shouldTerminate truncateFile
        it "rejects a negative offset" $ do
            sendRejected (PartOfFile (-1) 1000) `shouldReturn` ""
        it "rejects a negative length" $ do
            sendRejected (PartOfFile 2000 (-1)) `shouldReturn` ""
    describe "sendfileWithHeader" $ do
        it "sends an header and an entire file" $ do
            sendFileH EntireFile `shouldReturn` True
        it "sends an header and a part of file" $ do
            sendFileH (PartOfFile 2000 1000000) `shouldReturn` True
        it "sends only an header for a zero-length part" $ do
            sendFileH (PartOfFile 2000 0) `shouldReturn` True
        it "sends a large header and an entire file" $ do
            sendFileHLarge EntireFile `shouldReturn` True
        it "sends a large header and a part of file" $ do
            sendFileHLarge (PartOfFile 2000 1000000) `shouldReturn` True
        it "sends a header with many chunks" $ do
            sendFileCore EntireFile (Prelude.replicate 5000 "a\n") `shouldReturn` True
        it "terminates even if length is over" $ do
            shouldTerminate $ sendIllegalH (PartOfFile 2000 5000000)
        it "terminates even if offset is over" $ do
            shouldTerminate $ sendIllegalH (PartOfFile 5000000 6000000)
        -- On Windows, setFileSize throws an exception due to the
        -- access permission. The test case will be finished but it is
        -- not a right test.
        it "terminates even if the file is truncated" $ do
            shouldTerminate truncateFileH
        it "sends nothing for a negative offset" $ do
            sendRejectedH (PartOfFile (-1) 1000) `shouldReturn` ""
        it "sends nothing for a negative length" $ do
            sendRejectedH (PartOfFile 2000 (-1)) `shouldReturn` ""
        it "sends nothing if the file does not exist" $ do
            sendMissingH `shouldReturn` ""
  where
    fiveSecs = 5000000
    shouldTerminate body = do
        r <- timeout fiveSecs body
        when (r == Nothing) $ tr "TIMEOUT" >> dumpTrace
        r `shouldBe` Just ()

----------------------------------------------------------------

sendFile :: FileRange -> IO Bool
sendFile range = sendFileCore range []

sendFileH :: FileRange -> IO Bool
sendFileH range = sendFileCore range headers
  where
    headers = [
        BS.replicate 100 'a'
      , "\n"
      , BS.replicate 200 'b'
      , "\n"
      , BS.replicate 300 'c'
      , "\n"
      ]

sendFileHLarge :: FileRange -> IO Bool
sendFileHLarge range = sendFileCore range headers
  where
    headers = [
        BS.replicate 10000 'a'
      , "\n"
      , BS.replicate 20000 'b'
      , "\n"
      , BS.replicate 30000 'c'
      , "\n"
      ]

sendFileCore :: FileRange -> [ByteString] -> IO Bool
sendFileCore range headers = bracket setup teardown $ \(s2,_) -> do
    tr "reader:begin"
    runResourceT $ runConduit (sourceSocket s2 .| sinkFile outputFile)
    tr "reader:eof"
    runResourceT $ copyfile range
    !f1 <- BL.readFile outputFile
    !f2 <- BL.readFile expectedFile
    return $! (f1 == f2)
  where
    copyfile EntireFile = do
        -- of course, we can use <> here
        runConduit (sourceList headers .| sinkFile expectedFile)
        runConduit (sourceFile inputFile .| sinkAppendFile expectedFile)
    copyfile (PartOfFile off len) = do
        runConduit (sourceList headers .| sinkFile expectedFile)
        runConduit (sourceFile inputFile
                 .| CB.isolate (off' + len')
                 .| (CB.take off' >> sinkAppendFile expectedFile))
      where
        off' = fromIntegral off
        len' = fromIntegral len
    setup = do
        tr "setup:begin"
        (s1,s2) <- socketPair AF_UNIX Stream 0
        tr "setup:pair"
        tid <- forkIO $
            (tr "sender:begin" >> sf s1 >> tr "sender:sent")
            `finally` (tr "sender:close" >> sendEOF s1 >> tr "sender:closed")
        return (s2,tid)
      where
        sf s1
          | headers == [] = sendfile s1 inputFile range (tr "sender:hook")
          | otherwise     = sendfileWithHeader s1 inputFile range (tr "sender:hook") headers
        sendEOF = close
    teardown (s2,tid) = do
        tr "teardown:begin"
        close s2
        tr "teardown:closed"
        killThread tid
        tr "teardown:killed"
        removeFileIfExists outputFile
        removeFileIfExists expectedFile
    inputFile    = "test/inputFile"
    outputFile   = "test/outputFile"
    expectedFile = "test/expectedFile"

----------------------------------------------------------------

sendIllegal :: FileRange -> IO ()
sendIllegal range = sendIllegalCore range []

sendIllegalH :: FileRange -> IO ()
sendIllegalH range = sendIllegalCore range headers
  where
    headers = [
        BS.replicate 100 'a'
      , "\n"
      , BS.replicate 200 'b'
      , "\n"
      , BS.replicate 300 'c'
      , "\n"
      ]

sendIllegalCore :: FileRange -> [ByteString] -> IO ()
sendIllegalCore range headers = bracket setup teardown $ \(s2,_) -> do
    tr "reader:begin"
    runResourceT $ runConduit (sourceSocket s2 .| sinkFile outputFile)
    tr "reader:eof"
    return ()
  where
    setup = do
        tr "setup:begin"
        (s1,s2) <- socketPair AF_UNIX Stream 0
        tr "setup:pair"
        tid <- forkIO $
            (tr "sender:begin" >> sf s1 >> tr "sender:sent")
            `finally` (tr "sender:close" >> sendEOF s1 >> tr "sender:closed")
        return (s2,tid)
      where
        sf s1
          | headers == [] = sendfile s1 inputFile range (tr "sender:hook")
          | otherwise     = sendfileWithHeader s1 inputFile range (tr "sender:hook") headers
        sendEOF = close
    teardown (s2,tid) = do
        tr "teardown:begin"
        close s2
        tr "teardown:closed"
        killThread tid
        tr "teardown:killed"
        removeFileIfExists outputFile
    inputFile  = "test/inputFile"
    outputFile = "test/outputFile"

----------------------------------------------------------------

-- Returns what the peer received after sendfileWithHeader failed.
sendMissingH :: IO ByteString
sendMissingH = sendFailing $ \s ->
    sendfileWithHeader s "test/nonexistent" EntireFile (return ()) ["header"]

sendRejected :: FileRange -> IO ByteString
sendRejected range = sendFailing $ \s ->
    sendfile s "test/inputFile" range (return ())

sendRejectedH :: FileRange -> IO ByteString
sendRejectedH range = sendFailing $ \s ->
    sendfileWithHeader s "test/inputFile" range (return ()) ["header"]

-- Runs an action expected to throw and returns what the peer received.
sendFailing :: (Socket -> IO ()) -> IO ByteString
sendFailing act = bracket setup teardown $ \(s1,s2) -> do
    r <- try $ act s1
    case r of
        Left (_ :: IOException) -> return ()
        Right () -> expectationFailure "no exception"
    close s1
    BS.concat <$> runConduit (sourceSocket s2 .| CL.consume)
  where
    setup = socketPair AF_UNIX Stream 0
    teardown (_,s2) = close s2

----------------------------------------------------------------

truncateFile :: IO ()
truncateFile = truncateFileCore []

truncateFileH :: IO ()
truncateFileH = truncateFileCore headers
  where
    headers = [
        BS.replicate 100 'a'
      , "\n"
      , BS.replicate 200 'b'
      , "\n"
      , BS.replicate 300 'c'
      , "\n"
      ]

truncateFileCore :: [ByteString] -> IO ()
truncateFileCore headers = bracket setup teardown $ \(s2,_) -> do
    tr "reader:begin"
    runResourceT $ runConduit (sourceSocket s2 .| sinkFile outputFile)
    tr "reader:eof"
    return ()
  where
    setup = do
        runResourceT $ runConduit (sourceFile inputFile .| sinkFile tempFile)
        (s1,s2) <- socketPair AF_UNIX Stream 0
        ref <- newIORef (1 :: Int)
        tid <- forkIO (sf s1 ref `finally` sendEOF s1)
        return (s2,tid)
      where
        sf s1 ref
          | headers == [] = sendfile s1 tempFile range (hook ref)
          | otherwise     = sendfileWithHeader s1 tempFile range (hook ref) headers
        sendEOF = close
        hook ref = do
            n <- readIORef ref
            when (n == 10) $ setFileSize tempFile 900000
            writeIORef ref (n+1)
    teardown (s2,tid) = do
        tr "teardown:begin"
        close s2
        tr "teardown:closed"
        killThread tid
        tr "teardown:killed"
        removeFileIfExists tempFile
        removeFileIfExists outputFile
    inputFile  = "test/inputFile"
    tempFile   = "test/tempFile"
    outputFile = "test/outputFile"
    range = EntireFile

----------------------------------------------------------------

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = do
    exist <- doesFileExist file
    when exist $ removeFile file

sinkAppendFile :: MonadResource m
                  => FilePath
                  -> ConduitT ByteString Void m ()
sinkAppendFile fp = sinkIOHandle (openBinaryFile fp AppendMode)

----------------------------------------------------------------

-- Lightweight trace ring dumped only on timeout, to locate hangs
-- without perturbing timing (formatting happens in dumpTrace).
traceBuf :: IORef [(ThreadId, String)]
traceBuf = unsafePerformIO $ newIORef []
{-# NOINLINE traceBuf #-}

tr :: String -> IO ()
tr msg = do
    tid <- myThreadId
    atomicModifyIORef' traceBuf (\xs -> (Prelude.take 200 ((tid, msg) : xs), ()))

dumpTrace :: IO ()
dumpTrace = do
    xs <- atomicModifyIORef' traceBuf (\xs -> ([], xs))
    forM_ (Prelude.reverse (Prelude.take 80 xs)) $ \(t, m) ->
        System.IO.hPutStrLn stderr ("TRACE " ++ show t ++ " " ++ m)
    hFlush stderr
