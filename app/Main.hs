module Main where

import Language.Cobble.Prelude hiding (argument)
import Language.Cobble
import Language.Cobble.Types

import Options.Applicative
import Language.Cobble.Util.Polysemy.Time

main :: IO ()
main = runCobble =<< execParser (info (mainOpts <**> helper) mainInfo)
    where
        mainOpts = hsubparser (
              command "compile" (Compile <$> info compileOpts (progDesc "Compile source files to a datapack"))
            ) 
        mainInfo = idm

cobbleInfo :: InfoMod CobbleAction
cobbleInfo = mempty

runCobble :: CobbleAction -> IO ()
runCobble = \case
    Compile co -> runCompile co

runCompile :: CompileCmdOpts -> IO ()
runCompile CompileCmdOpts{compFiles, debug, packageName, description, target, traceLevel, ddumpTC, ddumpCore, skipCoreLint} = do
    let opts = CompileOpts {
            name=packageName
        ,   debug
        ,   description
        ,   target
        ,   ddumpTC
        ,   ddumpCore
        , skipCoreLint
        }
    case target of
        Racket -> do
            eRacketFile <- runTracePretty traceLevel $ runControllerC opts (compileToRacketFile compFiles)
            case eRacketFile of
                Left e -> failWithCompError e
                Right luaFile -> writeFileText (toString packageName <> ".rkt") luaFile

runTracePretty :: TraceLevel -> (Trace => a) -> a
runTracePretty lvl = runTraceStderrWith lvl pretty 
    where
        pretty lvl msg = logPrefix lvl <> msg <> "\ESC[0m"
        logPrefix = \case
            Warning -> "\ESC[38;2;255;128;0m\STX[WARNING]\ESC[38;2;255;176;0m\STX "
            Info    -> "\ESC[38;2;0;225;225m\STX[INFO]\ESC[38;2;136;225;225m\STX "
            Verbose -> "\ESC[38;2;0;175;175m\STX[VERBOSE]\ESC[38;2;70;200;200m\STX "
            Debug -> "\ESC[38;2;0;225;0m\STX[DEBUG]\ESC[38;2;140;255;140m\STX "
            DebugVerbose -> "\ESC[38;2;0;128;0m\STX[DEBUG VERBOSE]\ESC[38;2;80;200;80m\STX "
            DebugVeryVerbose -> "\ESC[38;2;0;100;0m\STX[DEBUG VERY VERBOSE]\ESC[38;2;40;145;40m\STX "
        

-- TODO
failWithCompError :: CompilationError -> IO a
failWithCompError e = fail $ "CompilationError: " <> show e

data CobbleAction = Compile CompileCmdOpts deriving (Show, Eq)

data CompileCmdOpts = CompileCmdOpts {
      compFiles :: [FilePath]
    , packageName :: Text
    , debug :: Bool
    , traceLevel :: TraceLevel
    , description :: Text
    , target :: Target
    , ddumpTC :: Bool
    , ddumpCore :: Bool
    , skipCoreLint :: Bool
    } deriving (Show, Eq)

compileOpts :: Parser CompileCmdOpts
compileOpts = CompileCmdOpts
    <$> some (argument str (metavar "SOURCEFILES"))
    <*> strOption (long "package-name" <> short 'p' <> metavar "NAME" <> help "The name of the package/datapack")
    <*> switch (long "debug" <> help "Debug mode keeps additional information at runtime")
    <*> option auto (long "log-level" <> metavar "LEVEL" <> value Info <> help "Controls how much information is logged (LogWarning | LogInfo | LogVerbose | LogDebug | LogDebugVerbose)")
    <*> strOption (long "description" <> short 'd' <> metavar "DESCRIPTION" <> help "The datapack description for the datapack's pack.mcmeta")
    <*> option auto (long "target" <> metavar "TARGET" <> value Racket <> help "Possible targets: racket")
    <*> switch (long "ddump-tc" <> help "Write the typechecker constraints to a file")
    <*> switch (long "ddump-core" <> help "Write the intermediate language Core to a file")
    <*> switch (long "skip-core-lint" <> help "Skip type checks for the internal language. Unless the compiler has a bug, this should always be safe.")
