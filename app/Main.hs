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
              command "compile" (Compile <$> info compileOpts (progDesc "Compile source files to a datapack"))
            ) 
        mainInfo = idm

cobbleInfo :: InfoMod CobbleAction
cobbleInfo = mempty

runCobble :: CobbleAction -> IO ()
runCobble = \case
    Compile co -> runCompile co

runCompile :: CompileCmdOpts -> IO ()
runCompile CompileCmdOpts{compFiles, debug, packageName, description, target, traceLevel, ddumpTC, ddumpLC, ddumpCPS, ddumpReduced, ddumpTL, ddumpAsm} = do
    let opts = CompileOpts {
            name=packageName
        ,   debug
        ,   description
        ,   target
        ,   ddumpTC
        ,   ddumpLC
        ,   ddumpCPS
        ,   ddumpReduced
        ,   ddumpTL    
        ,   ddumpAsm 
        }
    case target of
        MC117 -> do
            edatapackBS <- runTracePretty traceLevel $ runControllerC opts (timeToIO $ compileToDataPack compFiles)
            case edatapackBS of
                Left e -> failWithCompError e
                Right datapackBS -> writeFileLBS (toString packageName <> ".zip") datapackBS
        Lua -> do
            eluaFile <- runTracePretty traceLevel $ runControllerC opts (compileToLuaFile compFiles)
            case eluaFile of
                Left e -> failWithCompError e
                Right luaFile -> writeFileText (toString packageName <> ".lua") luaFile

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
    , ddumpLC :: Bool
    , ddumpCPS :: Bool
    , ddumpReduced :: Bool
    , ddumpTL :: Bool
    , ddumpAsm :: Bool
    } deriving (Show, Eq)

compileOpts :: Parser CompileCmdOpts
compileOpts = CompileCmdOpts
    <$> some (argument str (metavar "SOURCEFILES"))
    <*> strOption (long "package-name" <> short 'p' <> metavar "NAME" <> help "The name of the package/datapack")
    <*> switch (long "debug" <> help "Debug mode keeps additional information at runtime")
    <*> option auto (long "log-level" <> metavar "LEVEL" <> value Info <> help "Controls how much information is logged (LogWarning | LogInfo | LogVerbose | LogDebug | LogDebugVerbose)")
    <*> strOption (long "description" <> short 'd' <> metavar "DESCRIPTION" <> help "The datapack description for the datapack's pack.mcmeta")
    <*> option auto (long "target" <> metavar "TARGET" <> value MC117 <> help "Possible targets: mc-1.17, lua")
    <*> switch (long "ddump-tc" <> help "Write the typechecker constraints to a file")
    <*> switch (long "ddump-lc" <> help "Write the intermediate lambda calculus to a file")
    <*> switch (long "ddump-cps" <> help "Write the intermediate CPS to a file")
    <*> switch (long "ddump-reduced" <> help "Write the reduced intermediate CPS to a file")
    <*> switch (long "ddump-tl" <> help "Write the intermediate TopLevel CPS to a file")
    <*> switch (long "ddump-asm" <> help "Write the intermediate ASM to a file")
