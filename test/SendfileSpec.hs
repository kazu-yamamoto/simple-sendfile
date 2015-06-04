{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
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
import System.Posix.IO
import System.Posix.Types
import System.Posix.Files
import System.Process
import System.Timeout
import Foreign.Marshal.Alloc
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Test.Hspec

----------------------------------------------------------------

main :: IO ()
main = bracket
  (openFd verybigFile ReadWrite (Just 384) defaultFileFlags)
  (\fd -> closeFd fd >> removeLink verybigFile)
  (\fd -> hspec (spec fd))
    where
      verybigFile = "test/verybigFile"

spec :: Fd -> Spec
spec fd = do
    describe "sendfile" $ do
        it "sends an entire file" $ do
            sendFile EntireFile `shouldReturn` ExitSuccess
        it "sends a part of file" $ do
            sendFile (PartOfFile 2000 1000000) `shouldReturn` ExitSuccess
        it "terminates even if length is over" $ do
            shouldTerminate $ sendIllegal (PartOfFile 2000 5000000)
        it "terminates even if offset is over" $ do
            shouldTerminate $ sendIllegal (PartOfFile 5000000 6000000)
        it "terminates even if the file is truncated" $ do
            shouldTerminate truncateFile
    describe "sendfileWithHeader" $ do
        it "sends an header and an entire file" $ do
            sendFileH EntireFile `shouldReturn` ExitSuccess
        it "sends an header and a part of file" $ do
            sendFileH (PartOfFile 2000 1000000) `shouldReturn` ExitSuccess
        it "sends a large header and an entire file" $ do
            sendFileHLarge EntireFile `shouldReturn` ExitSuccess
        it "sends a large header and a part of file" $ do
            sendFileHLarge (PartOfFile 2000 1000000) `shouldReturn` ExitSuccess
        it "terminates even if length is over" $ do
            shouldTerminate $ sendIllegalH (PartOfFile 2000 5000000)
        it "terminates even if offset is over" $ do
            shouldTerminate $ sendIllegalH (PartOfFile 5000000 6000000)
        it "terminates even if the file is truncated" $ do
            shouldTerminate truncateFileH
    describe "sendVeryBigFile" $ do
        it "seeks to position 0" $ do
            fdSeek fd AbsoluteSeek 0 `shouldReturn` 0
        it "writes \"begin\\n\" at this offset" $ do
            fdWrite fd "begin\n" `shouldReturn` 6
        it ("seeks to position " ++ show eoff) $ do
            fdSeek fd AbsoluteSeek eoff `shouldReturn` eoff
        it "writes \"end\\n\" at this offset" $ do
            fdWrite fd "end\n" `shouldReturn` 4
        it ("produces a file of " ++ show maxs ++ " bytes as a result") $ do
            (fileSize `fmap` getFdStatus fd) `shouldReturn` maxs
        it "seeks again to position 0" $ do
            fdSeek fd AbsoluteSeek 0 `shouldReturn` 0
        it "reads \"begin\\n\" at this offset" $ do
            fdRead fd 6 `shouldReturn` ("begin\n", 6)
        it ("seeks again to position " ++ show eoff) $ do
            fdSeek fd AbsoluteSeek eoff `shouldReturn` eoff
        it "reads \"end\\n\" at this offset" $ do
            fdRead fd 4 `shouldReturn` ("end\n", 4)
        it "runs \"sendfile\" at 0 and checks data" $ do
            sendRecv fd 0 6 `shouldReturn` ("begin\n", 6)
        it ("runs \"sendfile\" at " ++ show eoff ++ " and checks data") $ do
            sendRecv fd eoff 4 `shouldReturn` ("end\n", 4)
        it ("runs \"sendfile\" for " ++ show bc32 ++ " bytes") $ do
            sendRecv fd fo32 bc32 `shouldReturn` ("", bc32)
  where
    fiveSecs = 5000000
    shouldTerminate body = timeout fiveSecs body `shouldReturn` Just ()
    maxs = 2^33-1
    eoff = maxs-4
    fo32 = 2^32-1 :: FileOffset
    bc32 = 2^32-1 :: ByteCount

----------------------------------------------------------------

sendFile :: FileRange -> IO ExitCode
sendFile range = sendFileCore range []

sendFileH :: FileRange -> IO ExitCode
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

sendFileHLarge :: FileRange -> IO ExitCode
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
    runResourceT $ sourceSocket s2 $$ sinkFile outputFile
    return ()
  where
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
    inputFile = "test/inputFile"
    outputFile = "test/outputFile"

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

----------------------------------------------------------------

sendRecv :: Fd -> FileOffset -> ByteCount -> IO (String, ByteCount)
sendRecv fd off size =
  bracket setup teardown (\x -> rcv x size 0 "" `catch` handler)
  where
    setup = do
      (s1,s2) <- socketPair AF_UNIX Stream 0
      tid <- forkIO (sendfileFd s1 fd
                                (PartOfFile (toInteger off) (toInteger size))
                                (return ()) `finally` sClose s1)
      buf <- mallocBytes 4096 :: IO (Ptr Word8)
      return (s2, tid, buf)
    teardown (s2, tid, buf) = do
      sClose s2
      killThread tid
      free buf
    rcv (s2, _, buf) sz got pfx = do
      rsize <- recvBuf s2 buf 4096
      let rlen = fromIntegral rsize :: ByteCount
      rstr <- peekCStringLen (castPtr buf :: Ptr CChar, 8)
      let npfx = if got == 0
                 then Prelude.filter (/= '\0') $
                      Prelude.take (min (fromIntegral sz) 8) rstr
                 else pfx
      -- Not sure why this `seq` is necessary but it is!
      got `seq` if rlen == sz || rlen <= 0
        then return (npfx, got + rlen)
        else rcv (s2, undefined, buf) (sz - rlen) (got + rlen) npfx
    handler :: IOException -> IO (String, ByteCount)
    handler e = return (show e, (-1))

