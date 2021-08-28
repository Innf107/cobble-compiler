{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Util.Polysemy.Fresh (
      Fresh(..)
    , freshVar
    , freshVar2
    , freshVar3
    
    , runFreshQNamesState
    , runFreshQNamesStateInitial
    
    , freshWithInternal
    ) where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName
import Language.Cobble.Types.LexInfo

import Data.Time
import Data.Time.Clock.POSIX

data Fresh u q m a where
    -- | Returns the time in **seconds** since Unix Epoch
    FreshVar :: u -> Fresh u q m q

makeSem ''Fresh

freshVar2 :: Members [Fresh a1 b1, Fresh a2 b2] r => a1 -> a2 -> Sem r (b1, b2)
freshVar2 x y = (,) <$> freshVar x <*> freshVar y

freshVar3 :: Members [Fresh a1 b1, Fresh a2 b2, Fresh a3 b3] r => a1 -> a2 -> a3 -> Sem r (b1, b2, b3)
freshVar3 x y z = (,,) <$> freshVar x <*> freshVar y <*> freshVar z

runFreshQNamesState :: Sem (Fresh (Text, LexInfo) QualifiedName : r) a -> Sem r a
runFreshQNamesState = runFreshQNamesStateInitial 0


freshWithInternal :: (Member (Fresh (Text, LexInfo) QualifiedName) r) => Sem (Fresh Text QualifiedName : r) a -> Sem r a
freshWithInternal = interpret \case
    FreshVar x -> freshVar (x, InternalLexInfo)

runFreshQNamesStateInitial :: Int -> Sem (Fresh (Text, LexInfo) QualifiedName : r) a -> Sem r a
runFreshQNamesStateInitial initial = evalState initial . reinterpret \case
    FreshVar (n, l) -> state (\i -> (UnsafeQualifiedName n (n <> "_" <> show i) l, i + 1))
