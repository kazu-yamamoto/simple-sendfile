module Network.Sendfile.Types where

data FileRange = EntireFile
               | PartOfFile {
                   rangeOffset :: Integer
                 , rangeLength :: Integer
                 }
