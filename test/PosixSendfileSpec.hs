{-# LANGUAGE OverloadedStrings #-}

module PosixSendfileSpec where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Binary as CB
import System.Directory
import System.Exit
import System.IO
import System.Posix.Files
import System.Posix.IO
import System.Posix.IO.Sendfile
import System.Posix.IO.Sendfile.Types
import System.Process
import Test.Hspec

----------------------------------------------------------------

spec :: Spec
spec =
    describe "sendfile" $ do
        it "sends an entire file" $
            sendFile EntireFile `shouldReturn` ExitSuccess
        it "sends a part of file" $
            sendFile (PartOfFile 2000 1000000) `shouldReturn` ExitSuccess

----------------------------------------------------------------

sendFile :: FileRange -> IO ExitCode
sendFile range = sendFileCore range


sendFileCore :: FileRange -> IO ExitCode
sendFileCore range = bracket setup teardown $ \(_, _) -> do
    runResourceT $ copyfile range
    system $ "cmp -s " ++ outputFile ++ " " ++ expectedFile
    where
    copyfile EntireFile =
        -- of course, we can use <> here
        sourceFile inputFile $$ sinkAppendFile expectedFile
    copyfile (PartOfFile off len) =
        sourceFile inputFile $= CB.isolate (off' + len')
                             $$ (CB.take off' >> sinkAppendFile
                                                 expectedFile)
      where
        off' = fromIntegral off
        len' = fromIntegral len
    setup = do
        fd1 <- openFd inputFile ReadOnly Nothing defaultFileFlags
        fd2 <- openFd outputFile WriteOnly (Just stdFileMode)
                                 defaultFileFlags
        sendfileFd fd2 fd1 range
        return (fd1, fd2)
    teardown (fd1, fd2) = do
      closeFd fd1
      closeFd fd2
      removeFileIfExists outputFile
      removeFileIfExists expectedFile
    inputFile    = "test/inputFile"
    outputFile   = "test/outputFile"
    expectedFile = "test/expectedFile"


----------------------------------------------------------------

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = do
    exist <- doesFileExist file
    when exist $ removeFile file

sinkAppendFile :: MonadResource m
                  => FilePath
                  -> Sink ByteString m ()
sinkAppendFile fp = sinkIOHandle (openBinaryFile fp AppendMode)
