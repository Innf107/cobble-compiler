{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Util.Polysemy.Time (
      Time
    , getTime
    , timeToIO
    , runTimePredefined
    , getTimeIO
    ) where

import Language.Cobble.Prelude

import Data.Time
import Data.Time.Clock.POSIX

data Time m a where
-- | Returns the time in **seconds** since Unix Epoch
    GetTime :: Time m Integer

makeSem ''Time

timeToIO :: Member (Embed IO) r => Sem (Time ': r) a -> Sem r a
timeToIO = interpret \case
    GetTime -> embed $ getTimeIO

runTimePredefined :: Integer -> Sem (Time ': r) a -> Sem r a
runTimePredefined t = interpret \case
    GetTime -> pure t

getTimeIO :: IO Integer
getTimeIO = round . nominalDiffTimeToSeconds <$> getPOSIXTime



