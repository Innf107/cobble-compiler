{-# LANGUAGE NoOverloadedLists #-}
module Lib where

-- TODO: Can we delete this module yet?

import Relude
import System.Directory
import System.FilePath

copyFileOrDirectory :: Bool -> FilePath -> FilePath -> IO ()
copyFileOrDirectory parents from to =
    doesFileExist from >>= \case
        True -> copyFile from to
        False -> doesDirectoryExist from >>= \case
            True -> do
                createDirectoryIfMissing parents to
                files <- filter (`notElem` ["..", "."]) <$> getDirectoryContents from
                forM_ files $ \file -> copyFileOrDirectory parents (from </> file) (to </> file)
            False -> fail $ "copyFileOrDirectory: File does not exist: " <> from
