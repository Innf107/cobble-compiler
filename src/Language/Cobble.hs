module Language.Cobble (
      compileAll
    , compileToDataPack
    , runControllerC
    , LogLevel(..)
    , Log(..)
    , CompilationError(..)
    , CompileOpts(..)
    , ControllerC
    ) where

import Language.Cobble.Prelude hiding ((<.>), readFile, writeFile)

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
import Language.Cobble.MCAsm.Types hiding (target)
import Language.Cobble.MCAsm.Types qualified as A

import Language.Cobble.MCAsm.McFunction

import Language.Cobble.Codegen.PrimOps
import Language.Cobble.Codegen.Types

import Data.Map qualified as M
import Data.List qualified as L
import Data.Text qualified as T
import System.FilePath qualified as FP

data CompilationError = LexError LexicalError
                      | ParseError ParseError
                      | QualificationError QualificationError
                      | AsmError McAsmError
                      | TypeError TypeError
                      | ModuleError ModuleError
                      | ControllerPanic Panic
                      deriving (Show, Eq)


type ControllerC r = Members '[Reader CompileOpts, Error CompilationError, Error Panic, FileSystem FilePath Text, Output Log] r

runControllerC :: CompileOpts 
               -> Sem '[Error Panic, Error CompilationError, Reader CompileOpts, FileSystem FilePath Text, Output Log, Embed IO] a
               -> IO ([Log], Either CompilationError a)
runControllerC opts = runM . outputToIOMonoidAssocR pure . fileSystemIO . runReader opts . runError . mapError ControllerPanic

data CompileOpts = CompileOpts {
      name::Text
    , debug::Bool
    , dataPackOpts::DataPackOptions
    , target::Target
    , ddumpAsm::Bool
    }

compileToDataPack :: (ControllerC r, Members '[Time] r) => [FilePath] -> Sem r LByteString
compileToDataPack files = do
    cmods <- compileAll files
    opts <- asks dataPackOpts
    makeDataPack opts cmods

compileAll :: (ControllerC r) => [FilePath] -> Sem r [CompiledModule]
compileAll files = do
    tokens <- traverse (\fn -> mapError LexError $ tokenizeError (toText fn) =<< readFile fn) files
    asts   <- traverse (\(ts, n) -> mapError ParseError $ fromEither $ parse (module_ (getModName n)) n ts) (zip tokens files)

    orderedMods :: [(S.Module 'SolveModules, [Text])] <- mapError ModuleError $ findCompilationOrder asts

    fmap join $ evalState (one ("prims", primModSig)) $ traverse compileAndAnnotateSig orderedMods

compileAndAnnotateSig :: (ControllerC r, Member (State (Map (S.Name 'QualifyNames) ModSig)) r)
                      => (S.Module 'SolveModules, [S.Name 'SolveModules])
                      -> Sem r [CompiledModule]
compileAndAnnotateSig (m, deps) = do
    annotatedMod :: (S.Module 'QualifyNames) <- S.Module
        <$> fromList <$> traverse (\d -> (makeQName d,) <$> getDep d) ("prims" : deps)
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
    let qualScopes = [Scope (makeQName $ S.moduleName m) mempty mempty mempty]
    let tcState = foldMap (\dsig -> TCState {
                    varTypes=exportedVars dsig
                ,   funReturnTypes=M.mapMaybe snd $ exportedFunctions dsig
                ,   funArgs=map snd . fst <$> exportedFunctions dsig
                })
                (xModule m)
    compEnv <- asks \CompileOpts{name, debug, target} -> CompEnv {nameSpace=name, debug, A.target=target}

    qMod  <- mapError QualificationError $ evalState qualScopes $ qualify m

    tcMod <- mapError TypeError $ evalState tcState $ typecheckModule qMod

    asmMod <- mapError AsmError $ evalState initialCompileState $ S.compile tcMod
    asks ddumpAsm >>= flip when (writeFile (show (A.moduleName asmMod) <> ".mamod") (showAsmDump asmMod))

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
    IfS _ _ _ _ _               -> mempty
    S.SetScoreboard () _ _ _ _  -> mempty
    LogS _ _                    -> mempty
    StatementX v _              -> absurd v

getModName :: FilePath -> Text
getModName = toText . FP.dropExtension . L.last . segments


primModSig :: ModSig
primModSig = ModSig {
        exportedFunctions = fmap (\(ps, ret, _) -> (ps,ret)) $ primOps 
            @'[Output Log, Writer [Instruction], Error Panic, State CompileState, Error McAsmError]
            -- The type application is only necessary to satisfy the type checker since the value depending on the type of 'r' is ignored
    ,   exportedVars = mempty
    ,   exportedStructs = fromList [("prims.Int", []), ("prims.Bool", []), ("prims.Entity", [])]
    }



showAsmDump :: A.Module -> Text
showAsmDump (A.Module imname iminstrs) = go imname iminstrs 0
    where
        go :: A.Name -> [Instruction] -> Int -> Text
        go mname minstr i = indent i $
            ["[" <> show mname <> "]"] ++ (minstr & map \case
                Section n is -> go n is (i + 1)
                inst -> show inst
                )
        indent :: Int -> [Text] -> Text
        indent i = T.unlines . map (T.replicate i "    " <>)


