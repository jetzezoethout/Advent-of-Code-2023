module ProcessFile
  ( processFile
  ) where

import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO
import           System.Environment (getArgs)
import           System.IO          (IOMode (ReadMode), hClose, openFile)

processFile :: (Text -> IO ()) -> IO ()
processFile action = do
  fileName <- head <$> getArgs
  handle <- openFile fileName ReadMode
  contents <- TIO.hGetContents handle
  action contents
  hClose handle
