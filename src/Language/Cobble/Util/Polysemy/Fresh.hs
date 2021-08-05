{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Util.Polysemy.Fresh (
      Fresh
    , freshVar
    , runFreshQNamesStateInitial
    ) where

import Language.Cobble.Prelude

import Language.Cobble.Shared

import Data.Time
import Data.Time.Clock.POSIX

data Fresh n m a where
    -- | Returns the time in **seconds** since Unix Epoch
    FreshVar :: n -> Fresh n m n

makeSem ''Fresh

runFreshQNamesStateInitial :: Int -> Sem (Fresh QualifiedName : r) a -> Sem r a
runFreshQNamesStateInitial initial = evalState initial . reinterpret \case
    FreshVar n -> state (\i -> (n +. show i, i + 1))

