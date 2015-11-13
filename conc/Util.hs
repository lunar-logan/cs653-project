module Util (
getFileSize

) where


import System.IO
import Control.Exception


-- Code taken from http://stackoverflow.com/questions/7878065/get-size-of-file-in-haskell
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler
                   $ bracket (openFile path ReadMode) (hClose) (\h -> do size <- hFileSize h
                                                                         return $ Just size)
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing