{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Util.Polysemy.Fresh (
      Fresh(..)
    , freshVar
    , freshVar2
    , freshVar3
    
    , runFresh
    , runFreshM
    , runFreshQNamesState
    , runFreshQNamesStateInitial
    
    , freshWithInternal
    , mapFresh
    ) where

import Language.Cobble.Prelude
import Language.Cobble.Types.QualifiedName
import Language.Cobble.Types.LexInfo

data Fresh u q m a where
    FreshVar :: u -> Fresh u q m q

makeSem ''Fresh

freshVar2 :: Members [Fresh a1 b1, Fresh a2 b2] r => a1 -> a2 -> Sem r (b1, b2)
freshVar2 x y = (,) <$> freshVar x <*> freshVar y

freshVar3 :: Members [Fresh a1 b1, Fresh a2 b2, Fresh a3 b3] r => a1 -> a2 -> a3 -> Sem r (b1, b2, b3)
freshVar3 x y z = (,,) <$> freshVar x <*> freshVar y <*> freshVar z

runFreshQNamesState :: Sem (Fresh (Text, LexInfo) QualifiedName : r) a -> Sem r a
runFreshQNamesState = runFreshQNamesStateInitial 0

mapFresh :: (Member (Fresh a b) r) => (b -> c) -> Sem (Fresh a c : r) x -> Sem r x
mapFresh f = interpret \case
    FreshVar x -> f <$> freshVar x

freshWithInternal :: (Member (Fresh (Text, LexInfo) QualifiedName) r) => Sem (Fresh Text QualifiedName : r) a -> Sem r a
freshWithInternal = interpret \case
    FreshVar x -> freshVar (x, InternalLexInfo)

runFreshQNamesStateInitial :: Int -> Sem (Fresh (Text, LexInfo) QualifiedName : r) a -> Sem r a
runFreshQNamesStateInitial initial = evalState initial . reinterpret \case
    FreshVar (n, l) -> state (\i -> (unsafeQualifiedName n (n <> "_" <> show i) l, i + 1))

runFresh :: (u -> q) -> Sem (Fresh u q : r) a -> Sem r a
runFresh f = runFreshM (pure . f)

runFreshM :: (u -> Sem r q) -> Sem (Fresh u q : r) a -> Sem r a
runFreshM f = interpret \case
    FreshVar x -> f x
