module Main where

import Cobble.Prelude hiding (argument, output)

import Cobble
import Cobble.Build

import Cobble.Types

import Cobble.Typechecker (TypeError(..), TypeContext(..)) -- Ugh

import Options.Applicative
import Cobble.Util.Polysemy.Time
import Cobble.Util.Polysemy.FileSystem


import Data.Binary (encodeFile)

import System.IO
import System.Process

main :: IO ()
main = runCobble =<< execParser (info (mainOpts <**> helper) mainInfo)
    where
        mainOpts = hsubparser (
                command "compile" (Compile <$> info compileOpts (progDesc "Compile source files to interfaces and object files"))
            <>  command "build" (Build <$> info buildOpts (progDesc "Build a project"))
            )
        mainInfo = idm

cobbleInfo :: InfoMod CobbleAction
cobbleInfo = mempty

runCobble :: CobbleAction -> IO ()
runCobble = \case
    Compile co -> runCompile co
    Build buildOpts -> runBuild buildOpts

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

runBuild :: BuildCmdOpts -> IO ()
runBuild BuildCmdOpts{projectFile, makeCommand, makeOptions} = do
    let opts = BuildOpts {
        projectFile
    }
    emakeWorkingDirectory <- runM $ runError $ runFileSystemGenericStringIO $ createMakefile opts
    case emakeWorkingDirectory of
        Left err -> failWithBuildError err
        Right makeWorkingDirectory -> callProcess (toString makeCommand) (fmap toString makeOptions <> ["-C", makeWorkingDirectory])

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
failWithCompError (TypeError err) = failWithTypeError err
failWithCompError e = do
    hPutStrLn stderr $ "\ESC[38;2;255;0;0m\STXCompilation Error: " <> show e <> "\ESC[0m\STX"
    exitFailure

failWithTypeError :: TypeError -> IO a
failWithTypeError (DifferentTCon con1 con2 li cxt) = typeError [
            "Cannot match type constructor '" <> originalName con1 <> "' with '" <> originalName con2 <> "'"
        ] li cxt
failWithTypeError (CannotUnify t1 t2 lexInfo context) = typeError [
            "Unable to unify types"
        ,   "    Expected: " <> ppType t1
        ,   "      Actual: " <> ppType t2
        ] lexInfo context
failWithTypeError (Occurs tvar ty li cxt) = typeError [
            "Unable to match an infinite type"
        ,   "    Expected: " <> ppType (TVar tvar)
        ,   "      Actual: " <> ppType ty
        ] li cxt
failWithTypeError (SkolBinding t1 t2 li context) = typeError [
            "Unable to unify with a rigid type variable"
        ,   "   Expected: " <> ppType t1
        ,   "     Actual: " <> ppType t2
        ] li context
failWithTypeError (Impredicative tv ty li cxt) = typeError [
            "Cannot instantiate type variable '" <> ppType (TVar tv) <> "' with the polymorphic type '" <> ppType ty <> "'"
        ,   "Cobble does not support impredicative polymorphism."
        ,   "Try wrapping your polymorphic type in a data constructor."
        ] li cxt


typeError :: Seq Text -> LexInfo -> Seq TypeContext -> IO a
typeError texts li context = do
    let msg = "\ESC[1m\STX" <> show li <> ": \ESC[38;2;255;0;0m\STXerror:\ESC[0m\ESC[1m\STX\n"
            <> unlines (toList texts)
            -- TODO: Include the actual code
            <> "\n"
            <> unlinesContext context
    hPutStrLn stderr (toString msg)
    exitFailure

unlinesContext :: Seq TypeContext -> Text
unlinesContext = unlines . toList . map (("• "<>) . showContext)

showContext :: TypeContext -> Text
showContext (WhenUnifying t1 t2) = "When unifying\n    Expected type:    " <> ppType t1
                                             <> "\n    with actual type: " <> ppType t2
showContext (WhenCheckingWanted c) = "When checking constraint: " <> show c
showContext (InDefinitionFor x ty) = "In the definition of: " <> originalName x <> " :: " <> ppType ty 

failWithBuildError :: BuildError -> IO a
failWithBuildError e = do
    hPutStrLn stderr $ "\ESC[38;2;255;0;0m\STXBuild Error: " <> show e <> "\ESC[0m\STX"
    exitFailure

data CobbleAction = Compile CompileCmdOpts 
                  | Build BuildCmdOpts
                  deriving (Show, Eq)

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

data BuildCmdOpts = BuildCmdOpts {
    projectFile :: FilePath
,   makeCommand :: Text
,   makeOptions :: [Text]
} deriving (Show, Eq)

buildOpts :: Parser BuildCmdOpts
buildOpts = BuildCmdOpts
    <$> option str (long "project-file" <> metavar "FILE" <> value "cobble.yaml" <> help "Use FILE for configuration instead of cobble.yaml")
    <*> option str (long "make-command" <> metavar "COMMAND" <> value "make" <> help "Use a custom command to run makefiles. Default: make")
    <*> option (words <$> str) (long "make-options" <> metavar "OPTIONS" <> value [] <> help "Pass additional options to make")
