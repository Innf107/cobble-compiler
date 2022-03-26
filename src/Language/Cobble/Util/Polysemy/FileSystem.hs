{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Util.Polysemy.FileSystem where

import Language.Cobble.Prelude

import Relude qualified as R
import Data.Maybe qualified as P
import Prelude qualified as P

import System.IO qualified as S
import System.Directory qualified as S
import System.FilePath qualified as S

-- | An Effect representing a file system, mostly used to simplify testing
-- The way in wich file descriptors are related to directories may depend on the implementation
--
-- laws:
-- writeFile fd fc >> readFile fd      = pure fc
-- writeFile fd fc >> writeFile fd fc' = writeFile fd fc'
-- writeFile fd fc >> deleteFile fd    = deleteFile fd
-- writeFile fd fc >> doesFileExist fd = pure fc
data FileSystem fd fc m a where
    ReadFile             :: fd -> FileSystem fd fc m fc
    WriteFile            :: fd -> fc -> FileSystem fd fc m ()
    DeleteFile           :: fd -> FileSystem fd fc m ()
    DeleteDirectory      :: fd -> FileSystem fd fc m ()
    DoesFileExist        :: fd -> FileSystem fd fc m Bool
    DoesDirectoryExist   :: fd -> FileSystem fd fc m Bool
    GetDirectoryContents :: fd -> FileSystem fd fc m [fd]
    CreateDirectory      :: fd -> FileSystem fd fc m ()

makeSem ''FileSystem

fileSystemIO :: (ToString fd, IsString fd, ToString fc, IsString fc, Member (Embed IO) r)
             => Sem (FileSystem fd fc ': r) a
             -> Sem r a
fileSystemIO = interpret \case
    ReadFile fd             -> embed (fromString <$> S.readFile (toString fd))
    WriteFile fd fc         -> embed (S.writeFile (toString fd) (toString fc))
    DeleteFile fd           -> embed (S.removeFile (toString fd))
    DeleteDirectory fd      -> embed (S.removeDirectory (toString fd))
    DoesFileExist fd        -> embed (S.doesFileExist (toString fd))
    DoesDirectoryExist fd   -> embed (S.doesDirectoryExist (toString fd))
    GetDirectoryContents fd -> embed (fmap fromString <$> S.getDirectoryContents (toString fd))
    CreateDirectory fd      -> embed (S.createDirectory (toString fd))

-- | A class representing File Descriptors.
-- Apart from 'segments', this is essentially
-- the same as 'Monoid', although the instances for '(./.)' and 'root'
-- might vary from those for 'mappend' and 'mempty' (eg. for 'FilePath'/'String'),
-- which explains why 'FileDescriptor' is not just a subclass of 'Monoid'.
--
-- laws:
-- segments root = []
-- segments (x ./. y) == segments x ++ segments y
--
-- monoid laws:
-- root ./. x = x
-- x ./. root = x
-- x ./. (y ./. z) = (x ./. y) ./. z
class (Eq fd, Eq (Segment fd)) => FileDescriptor fd where
    type Segment fd
    (./.) :: fd -> fd -> fd
    root :: fd
    segments :: fd -> [Segment fd]

infixr 5 ./.


instance FileDescriptor FilePath where
    type Segment FilePath = String
    x ./. y = R.intercalate "/" $ segments x ++ segments y
    root = ""
    segments = filter (/="/") . S.splitDirectories

