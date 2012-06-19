module SendfileSpec where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.Network
import Network.Sendfile
import Network.Socket
import System.Directory
import System.Exit
import System.Process
import Test.Hspec.ShouldBe

spec :: Spec
spec = do
    describe "sendfile" $ do
        it "sends an entire file" $ do
            runSendfile EntireFile `shouldReturn` ExitSuccess
        it "sends a part of file" $ do
            runSendfile (PartOfFile 2000 1000000) `shouldReturn` ExitSuccess

runSendfile :: FileRange -> IO ExitCode
runSendfile range = bracket setup teardown $ \(s2,_) -> do
    runResourceT $ sourceSocket s2 $$ sinkFile outputFile
    runResourceT $ copyfile range
    system $ "cmp -s " ++ outputFile ++ " " ++ expectedFile
  where
    copyfile EntireFile = sourceFile inputFile $$ sinkFile expectedFile
    copyfile (PartOfFile off len) =
        sourceFile inputFile $= CB.isolate (off' + len') $$
            (CB.take off' >> sinkFile expectedFile)
      where
        off' = fromIntegral off
        len' = fromIntegral len
    setup = do
        (s1,s2) <- socketPair AF_UNIX Stream 0
        tid <- forkIO (sendfile s1 inputFile range (return ()) `finally` sendEOF s1)
        return (s2,tid)
      where
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
