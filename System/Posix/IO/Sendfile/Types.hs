module System.Posix.IO.Sendfile.Types where

-- |
--  File range for 'sendfile'.

data FileRange = EntireFile
               | PartOfFile {
                   rangeOffset :: Integer
                 , rangeLength :: Integer
                 }
