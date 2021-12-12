{-# LANGUAGE TemplateHaskell #-}
{- An Effect for dumping compiler diagnostics to a file or stdout (e.g. --ddump-tc) -}
module Language.Cobble.Util.Polysemy.Dump (
        Dump(..)
    ,   dump

    ,   dontDump
    ,   dumpWith
    ,   dumpWhenWithM

    ,   dumpWithTo
    ,   dumpWhenWithTo
    ,   dumpWhenWithToM
    ) where

import Language.Cobble.Prelude hiding (writeFile)
import Language.Cobble.Util.Polysemy.FileSystem

import System.IO (hPutStrLn)

data Dump d m a where
    Dump :: d -> Dump d m ()

makeSem ''Dump

dumpWhenWithM :: Members '[FileSystem FilePath Text] r => Sem r Bool -> (d -> Text) -> FilePath -> Sem (Dump d : r) a -> Sem r a
dumpWhenWithM c pp fp a = c >>= \case
    True  -> dumpWith pp fp a
    False -> dontDump a

dumpWhenWithTo :: Members '[Embed m] r => (IO () -> m ()) -> Bool -> (d -> Text) -> Handle -> Sem (Dump d : r) a -> Sem r a
dumpWhenWithTo liftM True pp h a = dumpWithTo liftM pp h a
dumpWhenWithTo _ False _ _ a = dontDump a

dumpWhenWithToM :: Members '[Embed m] r => (IO () -> m ()) -> Sem r Bool -> (d -> Text) -> Handle -> Sem (Dump d : r) a -> Sem r a
dumpWhenWithToM liftM c pp h a = c >>= \case
    True -> dumpWithTo liftM pp h a 
    False -> dontDump a

dontDump :: Sem (Dump d : r) a -> Sem r a
dontDump = interpret \case
    Dump x -> pure ()

dumpWith :: (Members '[FileSystem FilePath Text] r) => (d -> Text) -> FilePath -> Sem (Dump d : r) a -> Sem r a
dumpWith pp fp = interpret \case
    Dump x -> writeFile fp (pp x)

dumpWithTo :: Members '[Embed m] r => (IO () -> m ()) -> (d -> Text) -> Handle -> Sem (Dump d : r) a -> Sem r a
dumpWithTo liftM pp h = interpret \case
        Dump x -> embed $ liftM $ hPutStrLn h (toString $ pp x)
