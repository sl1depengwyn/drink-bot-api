module System.Directory.Extended
  ( module System.Directory,
    removeIfExists,
  )
where

import Control.Exception
import System.Directory
import System.IO.Error hiding (catch)
import Prelude hiding (catch)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e