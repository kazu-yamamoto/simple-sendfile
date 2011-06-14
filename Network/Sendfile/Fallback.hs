module Network.Sendfile.Fallback (sendfile) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Enumerator
import Data.Enumerator.Binary as EB
import Data.Enumerator.List as EL
import Network.Sendfile.Types
import Network.Socket
import qualified Network.Socket.ByteString as SB

{-|
   Sendfile emulation using enumerator.
   Used system calls: open(), stat(), read(), send() and close().
-}
sendfile :: Socket -> FilePath -> FileRange -> IO ()
sendfile s fp EntireFile =
    run_ $ enumFile fp $$ sendIter s
sendfile s fp (PartOfFile off len) =
    run_ $ EB.enumFileRange fp (Just off) (Just len) $$ sendIter s

sendIter :: Socket -> Iteratee ByteString IO ()
sendIter s = do
    mb <- EL.head
    case mb of
        Nothing -> return ()
        Just bs -> do
            liftIO $ sendLoop s bs (BS.length bs)
            sendIter s

sendLoop :: Socket -> ByteString -> Int -> IO ()
sendLoop s bs len = do
    bytes <- SB.send s bs
    when (bytes /= len) $ sendLoop s (BS.drop bytes bs) (len - bytes)
