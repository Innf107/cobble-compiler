module Main where

import Cobble.Prelude hiding (argument, output)

import Cobble
import Cobble.Build

import Cobble.Syntax

import Cobble.Typechecker (TypeError(..), TypeContext(..)) -- Ugh
import Cobble.Core.Lint (CoreLintError(..))

import Options.Applicative
import Cobble.Util.Polysemy.Time
import Cobble.Util.Polysemy.FileSystem


import Data.Binary (encodeFile)
import Data.Text qualified as T

import System.IO as IO
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
runCompile CompileCmdOpts{compFile, interfaceFiles, output, target, traceTypes, ddumpRenamed, ddumpPostProcessed, ddumpTC, ddumpCore, skipCoreLint, lintAsError} = do
    let opts = CompileOpts {
            target
        ,   interfaceFiles
        ,   ddumpRenamed
        ,   ddumpPostProcessed
        ,   ddumpTC
        ,   ddumpCore
        ,   skipCoreLint
        ,   lintAsError
        }
    case target of
        Racket -> do
            eRacketFile <- runTracePretty traceTypes $ runControllerC opts (compileToRacketFile compFile)
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

runTracePretty :: Seq TraceType -> (Trace => a) -> a
runTracePretty traceTys = runTraceStderrWith pred pretty
    where
        -- There is no reason to recompute tracing predicates everytime
        -- so we cache them by binding them to variables.
        traceQualify = TraceQualify `elem` traceTys
        traceTC = TraceTC `elem` traceTys
        traceSolver = TraceSolver `elem` traceTys
        traceUnify = TraceUnify `elem` traceTys
        traceSubst = TraceSubst `elem` traceTys
        traceLower = TraceLower `elem` traceTys

        pred = \case
            TraceQualify -> traceQualify
            TraceCoreLint -> True
            TraceTC -> traceTC
            TraceSolver -> traceSolver
            TraceUnify -> traceUnify
            TraceSubst -> traceSubst
            TraceLower -> traceLower

        pretty ty msg = logPrefix ty <> msg <> "\ESC[0m"
        
        warningPrefix name = "\ESC[38;2;255;128;0m\STX[" <> name <> "]\ESC[38;2;255;176;0m\STX "        

        infoPrefix name = "\ESC[38;2;0;128;0m\STX[" <> name <> "]\ESC[38;2;80;200;80m\STX "

        logPrefix = \case
            TraceQualify -> infoPrefix "QUALIFY"
            TraceCoreLint -> warningPrefix "CORE LINT"
            TraceTC -> infoPrefix "TYPE CHECKER"
            TraceSolver -> infoPrefix "CONSTRAINT SOLVER"
            TraceUnify -> infoPrefix "UNIFY"
            TraceSubst -> infoPrefix "SUBST"
            TraceLower -> infoPrefix "LOWER"

failWithCompError :: CompilationError -> IO a
failWithCompError (Panic msg) = do
    hPutStrLn stderr $ toString $ "\ESC[38;2;255;0;0m\STXPANIC (the 'impossible' happened):\n" <> msg <> "\ESC[0m\STX"
    exitFailure
failWithCompError (TypeError err) = failWithTypeError err
failWithCompError (CoreLintError (MkCoreLintError msg)) = do
    hPutStrLn stderr $ toString $ "\ESC[38;2;255;0;0m\STXCore Lint Error:\n" <> msg <> "\ESC[0m\STX"
    exitFailure
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
        ,   "    Expected: " <> ppType (TUnif tvar)
        ,   "      Actual: " <> ppType ty
        ] li cxt
failWithTypeError (SkolBinding t1 t2 li context) = typeError [
            "Unable to unify with a rigid type variable"
        ,   "   Expected: " <> ppType t1
        ,   "     Actual: " <> ppType t2
        ] li context
failWithTypeError (Impredicative tv ty li cxt) = typeError [
            "Cannot instantiate type variable '" <> ppType (TUnif tv) <> "' with the polymorphic type '" <> ppType ty <> "'"
        ,   "Cobble does not support impredicative polymorphism."
        ,   "Try wrapping the polymorphic type in a data constructor."
        ] li cxt
failWithTypeError (NoInstanceFor constraint li cxt) = typeError [
            "No instance for '" <> show constraint <> "'"
        ] li cxt
failWithTypeError (InvalidRowHeadConstr ty li cxt) = typeError [
            "Invalid row head constructor '" <> show ty <> "'"
        ,   "Effects have to be fully applied, monomorphic effect type constructors."
        ] li cxt
failWithTypeError (RemainingRowFields fields ty1 ty2 li cxt) = 
        typeError (
            ("Row is missing fields:"
        <|  map (\(ty, _, _) -> "    " <> ppType ty) fields)
        <> ["Trying to unify rows '" <> ppType ty1 <> "' and '" <> ppType ty2 <> "'"]
        ) li cxt
failWithTypeError (MissingRowField field ty1 ty2 li cxt) = typeError [
            "Field '" <> originalName field <> "' is missing from row '" <> ppType ty2
        ,   "When matching against row '" <> ppType ty1 <> "'"
        ] li cxt

readSourceAtLexInfo :: LexInfo -> IO Text
readSourceAtLexInfo li@(LexInfo{startPos = SourcePos startLine startCol,endPos = SourcePos endLine endCol,file}) = do
    contents <- toText <$> IO.readFile (toString file)
    let contentLines :: Seq Text = fromList $ lines contents
    let relevantLines = drop (startLine - 1) $ take endLine contentLines
    

    let remainingPrefix = "\ESC[0m\STX"
    let importantPrefix = "\ESC[38;2;255;0;0m\ESC[1m\STX"
    let linePrefix = "\ESC[38;2;72;89;254m\ESC[1m\STX|    "
    case relevantLines of
        [line] -> do
            let initial = T.take (startCol - 1) line
            let important = T.drop (startCol - 1) $ T.take (endCol - 1) line
            let remaining = T.drop (endCol - 1) line
            pure $ linePrefix <> remainingPrefix <> initial 
                <> importantPrefix <> important 
                <> remainingPrefix <> remaining <> "\n"
        (firstLine :<| (lines :|> lastLine)) -> do
            let initial = T.take (startCol - 1) firstLine
            let importantFirst = T.drop (startCol - 1) $ firstLine
            let importantLastLine = T.take (endCol - 1) lastLine
            let remaining = T.drop (endCol - 1) lastLine
            pure $ linePrefix <> remainingPrefix <> initial
                <> importantPrefix <> importantFirst <> "\n"
                <> unlines (toList $ map ((linePrefix <> importantPrefix) <>) lines)
                <> linePrefix <> importantPrefix <> importantLastLine
                <> remainingPrefix <> remaining <> "\n\ESC[1m\STX" 

        _ -> pure $ "PANIC: UNABLE TO READ LINES FROM " <> show li



typeError :: Seq Text -> LexInfo -> Seq TypeContext -> IO a
typeError errorLines li context = do
    source <- readSourceAtLexInfo li
    let msg = "\ESC[1m\STX" <> show li <> ": \ESC[38;2;255;0;0m\STXerror:\ESC[0m\ESC[1m\STX\n"
            <> unlines (toList errorLines)
            <> unlinesContext context
            <> "\ESC[38;2;72;89;254m\ESC[1m\STX|\n"
            <> source
            <> "\ESC[38;2;72;89;254m\ESC[1m\STX|\ESC[0m\STX"
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
    , traceTypes :: Seq TraceType
    , target :: Target
    , ddumpRenamed :: Bool
    , ddumpPostProcessed :: Bool
    , ddumpTC :: Bool
    , ddumpCore :: Bool
    , skipCoreLint :: Bool
    , lintAsError :: Bool
    } deriving (Show, Eq)

compileOpts :: Parser CompileCmdOpts
compileOpts = CompileCmdOpts
    <$> (argument str (metavar "SOURCEFILE"))
    <*> (fromList <$> many (argument str (metavar "INTERFACES")))
    <*> option (Just <$> str) (long "output" <>  short 'o' <> metavar "FILE" <> value Nothing <> help "Write the output to FILE. An interface file will be written to FILE.cbi")
    <*> (fromList <$> many (option auto (long "trace" <> metavar "TYPE" <> help "Enable tracing for a given trace type. Possible values: tc, solver, unify, subst, lower")))
    <*> option auto (long "target" <> metavar "TARGET" <> value Racket <> help "Possible targets: racket")
    <*> switch (long "ddump-renamed" <> help "Write the renamed AST to a file")
    <*> switch (long "ddump-postprocessed" <> help "Write the postprocessed AST to a file")
    <*> switch (long "ddump-tc" <> help "Write the typechecker constraints to a file")
    <*> switch (long "ddump-core" <> help "Write the intermediate language Core to a file")
    <*> switch (long "skip-core-lint" <> help "Skip type checks for the internal language. Unless the compiler has a bug, this should always be safe.")
    <*> switch (long "lint-as-error" <> help "Treat core lint errors as compiler errors instead of warnings.")

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
