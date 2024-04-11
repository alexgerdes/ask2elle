module GhcLib.Analysis.Utility where

import Data.Foldable (foldrM)
import System.Directory
import System.FilePath
import System.IO (readFile')

-- | Given a base directory and a list of file names inside it, filter out non-haskell files,
-- | return a list of tuples where the first element is the file name and the second element is the content of the file.
getFilenameAndContent :: FilePath -> [String] -> IO [(String, String)]
getFilenameAndContent baseDir filenames = do
    foldrM
        ( \path acc ->
            if isExtensionOf "hs" path
                then do
                    content <- readFile' $ baseDir </> path
                    pure $ (takeBaseName path, content) : acc
                else pure acc
        )
        []
        filenames
