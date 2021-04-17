{-#LANGUAGE NoImplicitPrelude, ScopedTypeVariables, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
module Main where

import Language.Cobble.Prelude hiding (argument)
import Language.Cobble
import Language.Cobble.Types
import Language.Cobble.Packager

import Options.Applicative
import Language.Cobble.Util.Polysemy.Time

main :: IO ()
main = runCobble =<< execParser (info (mainOpts <**> helper) mainInfo)
    where
        mainOpts = hsubparser (
              command "compile" (Compile <$> info compileOpts (progDesc "Compile a source fileto a datapack directly"))
            ) 
        mainInfo = idm

cobbleInfo :: InfoMod CobbleAction
cobbleInfo = mempty

runCobble :: CobbleAction -> IO ()
runCobble = \case
    Compile co -> runCompile co

runCompile :: CompileCmdOpts -> IO ()
runCompile CompileCmdOpts{compFiles, debug, packageName, description, logLevel, target} = do
    let opts = CompileOpts {
            name=packageName
        ,   debug
        ,   dataPackOpts = DataPackOptions {
                name=packageName
            ,   description
            ,   target
            }            
        ,   target       
        }
    (logs, edatapackBS) <- runControllerC opts (timeToIO $ compileToDataPack compFiles)
    traverse_ (printLog logLevel) logs
    case edatapackBS of
        Left e -> failWithCompError e
        Right datapackBS -> writeFileLBS (toString packageName <> ".zip") datapackBS

printLog :: LogLevel -> Log -> IO ()
printLog maxLevel (Log lvl o) = when (lvl <= maxLevel)
    $ putTextLn (logPrefix lvl <> o <> "\ESC[0m")
    where
        logPrefix = \case
            LogWarning -> "\ESC[38;2;255;128;0m\STX[WARNING] "
            LogInfo    -> "\ESC[38;2;0;225;225m\STX[INFO] "
            LogVerbose -> "\ESC[38;2;0;128;128m\STX[VERBOSE] "
            LogDebug -> "\ESC[38;2;0;225;0m\STX[DEBUG] "
            LogDebugVerbose -> "\ESC[38;2;0;128;0m\STX[DEBUG VERBOSE] "
            LogDebugVeryVerbose -> "\ESC[38;2;0;100;0m\STX[DEBUG VERY VERBOSE] "

-- TODO
failWithCompError :: CompilationError -> IO a
failWithCompError e = fail $ "CompilationError: " <> show e

data CobbleAction = Compile CompileCmdOpts deriving (Show, Eq)

data CompileCmdOpts = CompileCmdOpts {
      compFiles :: [FilePath]
    , packageName :: Text
    , debug :: Bool
    , logLevel :: LogLevel
    , description :: Text
    , target :: Target
    } deriving (Show, Eq)

compileOpts :: Parser CompileCmdOpts
compileOpts = CompileCmdOpts
    <$> some (argument str (metavar "SOURCEFILES"))
    <*> strOption (long "package-name" <> short 'p' <> metavar "NAME" <> help "The name of the package/datapack")
    <*> switch (long "debug" <> help "Debug mode keeps additional information at runtime")
    <*> option auto (long "log-level" <> metavar "LEVEL" <> value LogInfo <> help "Controls how much information is logged (LogWarning | LogInfo | LogVerbose | LogDebug | LogDebugVerbose)")
    <*> strOption (long "description" <> short 'd' <> metavar "DESCRIPTION" <> help "The datapack description for the datapack's pack.mcmeta")
    <*> option parseTarget (long "target" <> short 't' <> metavar "TARGET" <> help "The Minecraft version targeted by this datapack")

parseTarget :: ReadM Target
parseTarget = eitherReader $ \case
    "1.16" -> pure target116
    "1.17" -> pure target117
    _ -> Left "Supported targets are: 1.16 | 1.17"
