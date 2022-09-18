module Cobble.Config (
    Config (..),
    getConfig,
    defaultConfig,
    configRef,
    modifyConfig,
) where

{- This module contains global configuration for debug output such as
disambiguated names and constructor kinds.

Keeping this in a *global* configuration might sound dangerous, but this is only the case
for convenience. The compiler will only ever change the configuration before actually running, so
the configuration is effectively immutable.
-}
import Cobble.Prelude
import GHC.IO (unsafePerformIO)

data Config = Config
    { disambiguateNames :: Bool
    , disambiguateModules :: Bool
    , printKinds :: Bool
    }
    deriving (Show)

defaultConfig :: Config
defaultConfig =
    Config
        { disambiguateNames = False
        , disambiguateModules = False
        , printKinds = False
        }

configRef :: IORef Config
configRef = unsafePerformIO $ newIORef defaultConfig
{-# NOINLINE configRef #-}

modifyConfig :: (Config -> Config) -> IO ()
modifyConfig = modifyIORef' configRef

getConfig :: () -> Config
getConfig () = unsafePerformIO $ readIORef configRef
