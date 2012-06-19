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
runSendfile range = bracket setup teardown body
  where
    setup = do
        (s1,s2) <- socketPair AF_UNIX Stream 0
        tid <- forkIO (sendfile s1 inputFile range (return ()) `finally` sendEOF s1)
        return (s2,tid)
      where
        sendEOF = sClose
    body (s2,_) = do
        runResourceT $ sourceSocket s2 $$ sinkFile outputFile
        let run = case range of
                EntireFile -> sourceFile inputFile $$ sinkFile expectedFile
                PartOfFile start end -> do
                    let start' = fromIntegral start
                        end' = fromIntegral end
                    sourceFile inputFile $= CB.isolate (start' + end') $$ (CB.take start' >> sinkFile expectedFile)
        runResourceT run
        system $ "cmp -s " ++ outputFile ++ " " ++ expectedFile
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
