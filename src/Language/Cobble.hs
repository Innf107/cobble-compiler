module Language.Cobble where

import Language.Cobble.Prelude hiding ((<.>), readFile)

import Language.Cobble.Compiler as S
import Language.Cobble.Types as S
import Language.Cobble.Parser.Tokenizer as S
import Language.Cobble.Parser as S
import Language.Cobble.Qualifier as S
import Language.Cobble.Packager
import Language.Cobble.ModuleSolver
import Language.Cobble.Util.Polysemy.Time
import Language.Cobble.Util.Polysemy.FileSystem
import Language.Cobble.Util
import Language.Cobble.Shared

import Language.Cobble.Prelude.Parser (ParseError, parse)

import Language.Cobble.Typechecker as TC

import Language.Cobble.MCAsm.Compiler as A
import Language.Cobble.MCAsm.Types as A

import Language.Cobble.MCAsm.McFunction

import Data.Map qualified as M
import Data.List qualified as L
import System.FilePath qualified as FP

data CompilationError = LexError LexicalError
                      | ParseError ParseError
                      | QualificationError QualificationError
                      | AsmError McAsmError
                      | TypeError TypeError
                      | ModuleError ModuleError
                      | ControllerPanic Panic
                      deriving (Show, Eq)


type ControllerC r = Members '[Reader CompileOpts, Error CompilationError, Error Panic, FileSystem FilePath Text] r

runControllerC :: CompileOpts 
               -> Sem '[Error Panic, Error CompilationError, Reader CompileOpts, FileSystem FilePath Text, Embed IO] a
               -> IO (Either CompilationError a)
runControllerC opts = runM . fileSystemIO . runReader opts . runError . mapError ControllerPanic

-- TODO
data CompileOpts = CompileOpts {
      name::Text
    , fileName::Text
    , debug::Bool
    }

{-
--TODO: Use Module
compileFileToDatapack :: CompileOpts -> Text -> IO (Either CompilationError LByteString)
compileFileToDatapack opts@CompileOpts{debug=debug_,name=name_} content = do
    compMods <- pure do
        toks     <- first LexError $ tokenize (fileName opts) content
        modAst   <- first ParseError $ parse (module_ name_) (toString (fileName opts)) toks
        qualAst  <- first QualificationError $ qualify modAst
        tcAst    <- first TypeError $ runModuleTypecheck qualAst
        asm      <- join $ run $ runError $ mapError CompilerError $ runError $ mapError AsmError $ evalState initialCompileState $ S.compile tcAst
        first AsmError $ run $ runReader A.CompEnv {debug=debug_,nameSpace=name_} $ runError $ evalState A.initialCompState $ A.compile [asm]
    case compMods of
        Left e -> pure (Left e)
        Right cms -> Right <$> (runM $ timeToIO $ makeDataPack (dataPackOptions name_ "Created with Cobble") cms)

compileFully :: NameSpace -> Bool -> [S.Module 'Typecheck] -> Either CompilationError [CompiledModule]
compileFully nameSpace debug mods = do
    tmods <- first TypeError $ run $ runError $ evalState initialTCState $ traverse (TC.typecheckModule) mods
    asmMods <- join $ first CompilerError $ fmap (first AsmError) $
        run $ runError @CompilerError $ runError @McAsmError $ evalState S.initialCompileState $ traverse S.compile tmods
    first AsmError $ run $ runReader (CompEnv debug nameSpace) $ evalState A.initialCompState $ runError $ A.compile asmMods

compileToFunctionsAtPath :: FilePath -> NameSpace -> Bool -> [S.Module 'Typecheck] -> Either CompilationError (IO ())
compileToFunctionsAtPath path nameSpace debug mods = compileFully nameSpace debug mods <&> \cmods -> do
        createDirectory (path </> toString nameSpace)
        createDirectory (path </> toString nameSpace </> "functions")
        for_ cmods \CompiledModule{compModName, compModInstructions} ->
            writeFileText (path </> toString nameSpace </> "functions" </> show compModName <.> "mcfunction") compModInstructions
-}
compileAll :: (ControllerC r) => [FilePath] -> Sem r [CompiledModule]
compileAll files = do
    tokens <- traverse (\fn -> mapError LexError $ tokenizeError (toText fn) =<< readFile fn) files
    asts   <- traverse (\(ts, n) -> mapError ParseError $ fromEither $ parse (module_ (getModName n)) n ts) (zip tokens files)

    orderedMods :: [(S.Module 'SolveModules, [Text])] <- mapError ModuleError $ findCompilationOrder asts

    fmap join $ evalState mempty $ traverse compileAndAnnotateSig orderedMods

compileAndAnnotateSig :: (ControllerC r, Member (State (Map (S.Name 'QualifyNames) ModSig)) r)
                      => (S.Module 'SolveModules, [S.Name 'SolveModules])
                      -> Sem r [CompiledModule]
compileAndAnnotateSig (m, deps) = do
    annotatedMod :: (S.Module 'QualifyNames) <- S.Module
        <$> fromList <$> traverse (\d -> (makeQName d,) <$> getDep d) deps
        <*> pure (S.moduleName m)
        <*> pure (map coercePass (moduleStatements m))
    (compMod, sig) <- compileWithSig annotatedMod
    modify (insert (S.moduleName m) sig)
    pure compMod

getDep :: (ControllerC r, Member (State (Map (S.Name 'QualifyNames) ModSig)) r)
       => S.Name 'SolveModules
       -> Sem r ModSig
getDep n = maybe (throw (ModuleDependencyNotFound n)) pure =<< gets (lookup n)

compileWithSig :: (ControllerC r)
               => S.Module 'QualifyNames
               -> Sem r ([CompiledModule], ModSig)
compileWithSig m = do
    let qualScopes =
            map (\(dname, dsig) -> Scope {
                    _prefix=dname
                ,   _typeNames=map unqualifyName $ keys (exportedStructs dsig)
                ,   _varFunNames=map unqualifyName $ keys (exportedVars dsig) ++ keys (exportedFunctions dsig)
                ,   _typeKinds=todo mempty})
                (M.toList $ xModule m)
            ++ [Scope (makeQName $ S.moduleName m) mempty mempty mempty]
    let tcState = foldMap (\dsig -> TCState {
                    varTypes=exportedVars dsig
                ,   funReturnTypes=M.mapMaybe snd $ exportedFunctions dsig
                ,   funArgs=map snd . fst <$> exportedFunctions dsig
                })
                (xModule m)
    compEnv <- asks \CompileOpts{name, debug} -> CompEnv {nameSpace=name, debug}
    qMod  <- mapError QualificationError $ evalState qualScopes $ qualify m
    tcMod <- mapError TypeError $ evalState tcState $ typecheckModule qMod
    asmMod <- mapError AsmError $ evalState initialCompileState $ S.compile tcMod
    compMods <- mapError AsmError $ evalState initialCompState $ runReader compEnv $ A.compile [asmMod]
    let sig = extractSig tcMod
    pure (compMods, sig)

-- TODO: Move to own module
extractSig :: S.Module 'Codegen -> ModSig
extractSig (S.Module _deps _n sts) = foldMap makePartialSig sts

makePartialSig :: S.Statement 'Codegen -> ModSig
makePartialSig = \case
    Decl () _ n _ e             -> mempty {exportedVars = one (n, getType e)}
    DefVoid () _ n ps _         -> mempty {exportedFunctions = one (n,(ps, Nothing))}
    DefFun () _ n ps _ _ t      -> mempty {exportedFunctions = one (n,(ps, Just t))}
    DefStruct () _ n fs         -> mempty {exportedStructs = one (n, fs)}
    CallFun () _ _ _            -> mempty
    Import () _ _               -> mempty
    Assign () _ _ _             -> mempty
    While () _ _ _              -> mempty
    S.SetScoreboard () _ _ _ _  -> mempty
    StatementX v _              -> absurd v

getModName :: FilePath -> Text
getModName = toText . FP.dropExtension . L.last . segments


