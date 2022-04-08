module Cobble.Build where

import Cobble.Prelude

import Cobble.Util.Polysemy.FileSystem

data BuildOpts = BuildOpts {
    projectFile :: FilePath
} deriving (Show, Eq)

createMakeFile :: Members '[FileSystem FilePath Text] r => BuildOpts -> Sem r FilePath
createMakeFile = do
    undefined

data ProjectOpts = ProjectOpts {
    projectName :: Text
,   sourceDir :: FilePath
,   outDir :: FilePath
} deriving (Show, Eq)

parseProjectOpts :: Text -> ProjectOpts
parseProjectOpts = undefined


