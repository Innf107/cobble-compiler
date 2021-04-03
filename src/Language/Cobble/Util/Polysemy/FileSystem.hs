{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Util.Polysemy.FileSystem where

import Language.Cobble.Prelude

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
    GetDirectoryContents fd -> embed (map fromString <$> S.getDirectoryContents (toString fd))
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
    x ./. y = intercalate "/" $ segments x ++ segments y
    root = ""
    segments = filter (/="/") . S.splitDirectories

-- | A pure representation of a file system for use with 'fileSystemToPureState'
data PureFS fd fc = PureFSFile fd fc
                  | PureFSDirectory fd [PureFS fd fc]
                  deriving (Show, Eq)

alterFilePure :: forall fd fc a. (FileDescriptor fd)
              => (Maybe fc -> (Maybe fc, a))
              -> fd
              -> PureFS fd fc
              -> (PureFS fd fc, Maybe a)
alterFilePure f fd = first (fromMaybe (PureFSDirectory root [])) . alterFilePureInner root f fd
    where
        alterFilePureInner :: fd
                          -> (Maybe fc -> (Maybe fc, a))
                          -> fd
                          -> PureFS fd fc
                          -> (Maybe (PureFS fd fc), Maybe a)
        alterFilePureInner fdTrail f fd = \case
            fs@(PureFSFile fd' fc)
                | fd == fdTrail ./. fd' -> let (mfc', res) = f (Just fc) in
                    (PureFSFile fd' <$> mfc', Just res)
                | otherwise -> (Just fs, Nothing)
            fs@(PureFSDirectory fd' chs)
                | fd == fdTrail ./. fd' -> (Just fs, Nothing)
                | segments (fdTrail ./. fd') == P.init (segments fd) -> let (newch, res) = f Nothing in
                    (Just (PureFSDirectory fd' (maybeToList (PureFSFile fd <$> newch) ++ chs)), Just res)
                | otherwise ->
                    let results = map (alterFilePureInner (fdTrail ./. fd') f fd) chs
                        chs' = catMaybes (map fst results)
                        mresult = getFirst (foldMap (First . snd) results)
                    in
                    (Just (PureFSDirectory fd' chs'), mresult)

readFilePure :: (FileDescriptor fd) => fd -> PureFS fd fc -> Maybe fc
readFilePure = fmap (join . snd) . alterFilePure (\c -> (c, c))

writeFilePure :: (FileDescriptor fd) => fd -> fc -> PureFS fd fc -> (PureFS fd fc, Bool)
writeFilePure fd fc = second (maybe False (const True)) . alterFilePure (\_ -> (Just fc, True)) fd

deleteFilePure :: (FileDescriptor fd) => fd -> PureFS fd fc -> (PureFS fd fc, Bool)
deleteFilePure fd = second (maybe False (const True)) . alterFilePure (\_ -> (Nothing, True)) fd

doesFileExistPure :: (FileDescriptor fd) => fd -> PureFS fd fc -> Bool
doesFileExistPure fd = fromMaybe False . snd . alterFilePure (\x -> (x, maybe False (const True) x)) fd

data PureFSError fd = FileDoesNotExist fd
                    | DirectoryDoesNotExist fd
                    | ParentDirectoryDoesNotExist fd
                    deriving (Show, Eq)

fileSystemToPureState :: (FileDescriptor fd, Members [Error (PureFSError fd), State (PureFS fd fc)] r)
                  => Sem (FileSystem fd fc ': r) a
                  -> Sem r a
fileSystemToPureState = interpret \case
    ReadFile fd      -> maybe (throw (FileDoesNotExist fd)) pure =<< gets (readFilePure fd)
    WriteFile fd fc  -> bool (throw (ParentDirectoryDoesNotExist fd)) pass =<< state (swap . writeFilePure fd fc)
    DeleteFile fd    -> bool (throw (FileDoesNotExist fd)) pass =<< state (swap . deleteFilePure fd)
    DoesFileExist fd -> gets (doesFileExistPure fd)
    