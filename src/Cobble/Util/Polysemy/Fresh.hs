{-# LANGUAGE TemplateHaskell #-}
module Cobble.Util.Polysemy.Fresh (
      Fresh(..)
    , fresh
    , runFreshInt
    , freshGenerated
    , freshLocal

    , Unique
) where

import Cobble.Prelude
import Cobble.Syntax.QualifiedName
import Cobble.Syntax.LexInfo

data Fresh u m a where
    Fresh :: Fresh u m u

makeSem ''Fresh

runFreshInt :: Sem (Fresh Int : r) a -> Sem r a
runFreshInt = evalState (0 :: Int) . reinterpret \case
    Fresh -> state (\s -> (s, s + 1))

-- TODO: Temporary definition to make switching to actual uniques easier
type Unique = Int

freshGenerated :: Members '[Fresh Unique] r => Text -> Sem r QualifiedName
freshGenerated x = do
    u <- fresh
    pure (UnsafeQualifiedName x (GeneratedQName u))

freshLocal :: Members '[Fresh Unique] r => Text -> Sem r QualifiedName
freshLocal x = do
    u <- fresh
    pure (UnsafeQualifiedName x (LocalQName u))

