{-# LANGUAGE TemplateHaskell #-}
{- An Effect for dumping compiler diagnostics to a file or stdout (e.g. --ddump-tc) -}
module Language.Cobble.Util.Polysemy.Dump (
        Dump(..)
    ,   dump

    ,   dontDump
    ,   dumpWith
    ,   dumpWhenWithM
    ) where

import Language.Cobble.Prelude hiding (writeFile)
import Language.Cobble.Util.Polysemy.FileSystem

data Dump d m a where
    Dump :: d -> Dump d m ()

makeSem ''Dump

dumpWhenWithM :: Members '[FileSystem FilePath Text] r => Sem r Bool -> (d -> Text) -> FilePath -> Sem (Dump d : r) a -> Sem r a
dumpWhenWithM c pp fp a = c >>= \case
    True  -> dumpWith pp fp a
    False -> dontDump a

dontDump :: Sem (Dump d : r) a -> Sem r a
dontDump = interpret \case
    Dump x -> pure ()

dumpWith :: (Members '[FileSystem FilePath Text] r) => (d -> Text) -> FilePath -> Sem (Dump d : r) a -> Sem r a
dumpWith pp fp = interpret \case
    Dump x -> writeFile fp (pp x)
