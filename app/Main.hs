module Main where

import Cobble.Prelude hiding (argument, output)
import Cobble
import Cobble.Types

import Options.Applicative
import Cobble.Util.Polysemy.Time

import Data.Binary (encodeFile)

import System.IO

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
runCompile CompileCmdOpts{compFile, interfaceFiles, output, target, traceLevel, ddumpTC, ddumpCore, skipCoreLint} = do
    let opts = CompileOpts {
            target
        ,   interfaceFiles
        ,   ddumpTC
        ,   ddumpCore
        ,   skipCoreLint
        }
    case target of
        Racket -> do
            eRacketFile <- runTracePretty traceLevel $ runControllerC opts (compileToRacketFile compFile)
            case eRacketFile of
                Left e -> failWithCompError e
                Right (racketFile, interface) -> do
                    let (racketOutPath, ifaceOutPath) = case output of
                            Nothing -> (dropExtension compFile <> ".rkt", dropExtension compFile <> ".cbi")
                            Just path -> (path, dropExtension path <> ".cbi")
                    writeFileText racketOutPath racketFile
                    encodeFile ifaceOutPath interface

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
        

failWithCompError :: CompilationError -> IO a
failWithCompError (Panic msg) = do
    hPutStrLn stderr $ toString $ "\ESC[38;2;255;0;0m\STXPANIC (the 'impossible' happened):\n" <> msg <> "\ESC[0m\STX"
    exitFailure
failWithCompError e = do
    hPutStrLn stderr $ "CompilationError: " <> show e
    exitFailure

data CobbleAction = Compile CompileCmdOpts deriving (Show, Eq)

data CompileCmdOpts = CompileCmdOpts {
      compFile :: FilePath
    , interfaceFiles :: Seq FilePath
    , output :: Maybe FilePath
    , traceLevel :: TraceLevel
    , target :: Target
    , ddumpTC :: Bool
    , ddumpCore :: Bool
    , skipCoreLint :: Bool
    } deriving (Show, Eq)

compileOpts :: Parser CompileCmdOpts
compileOpts = CompileCmdOpts
    <$> (argument str (metavar "SOURCEFILE"))
    <*> (fromList <$> many (argument str (metavar "INTERFACES")))
    <*> option (Just <$> str) (long "output" <>  short 'o' <> metavar "FILE" <> value Nothing <> help "Write the output to FILE. An interface file will be written to FILE.cbi")
    <*> option auto (long "log-level" <> metavar "LEVEL" <> value Info <> help "Controls how much information is logged (LogWarning | LogInfo | LogVerbose | LogDebug | LogDebugVerbose)")
    <*> option auto (long "target" <> metavar "TARGET" <> value Racket <> help "Possible targets: racket")
    <*> switch (long "ddump-tc" <> help "Write the typechecker constraints to a file")
    <*> switch (long "ddump-core" <> help "Write the intermediate language Core to a file")
    <*> switch (long "skip-core-lint" <> help "Skip type checks for the internal language. Unless the compiler has a bug, this should always be safe.")
