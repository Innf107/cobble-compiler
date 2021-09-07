module Language.Cobble (
      compileAll
    , compileContents
    , compileToDataPack
    , compileContentsToDataPack
    , runControllerC
    , LogLevel(..)
    , Log(..)
    , CompilationError(..)
    , CompileOpts(..)
    , ControllerC
    ) where

import Language.Cobble.Prelude hiding ((<.>), readFile, writeFile)

import Language.Cobble.Types as S
import Language.Cobble.Parser.Tokenizer as S
import Language.Cobble.Parser as S
import Language.Cobble.Qualifier as S
import Language.Cobble.SemAnalysis as S
import Language.Cobble.Packager
import Language.Cobble.ModuleSolver
import Language.Cobble.Util.Polysemy.Time
import Language.Cobble.Util.Polysemy.FileSystem
import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Util

import Language.Cobble.Prelude.Parser (ParseError, parse)

import Language.Cobble.Typechecker as TC

import Language.Cobble.MCAsm.Types
import Language.Cobble.MCAsm.Types qualified as A

import Language.Cobble.McFunction.Types

import Language.Cobble.Codegen.PrimOps

import Language.Cobble.Codegen.CobbleToLC as C2LC
import Language.Cobble.Codegen.LCToBasicCPS as LC2CPS
import Language.Cobble.Codegen.BasicCPSToTopLevelCPS as CPS2TL
import Language.Cobble.Codegen.TopLevelCPSToMCAsm as TL2ASM
import Language.Cobble.Codegen.MCAsmToMCFunction as ASM2MC

import Language.Cobble.LC.Types as LC
import Language.Cobble.LC.PrettyPrint as LC

import Language.Cobble.CPS.Basic.Types
import Language.Cobble.CPS.TopLevel.Types

import Data.Map qualified as M
import Data.List qualified as L
import Data.Text qualified as T
import System.FilePath qualified as FP

import qualified Control.Exception as Ex

data CompilationError = LexError LexicalError
                      | ParseError ParseError
                      | QualificationError QualificationError
                      | SemanticError SemanticError
                      | TypeError TypeError
                      | ModuleError ModuleError
                      | Panic Text
                      deriving (Show, Eq)


type ControllerC r = Members '[Reader CompileOpts, Error CompilationError, FileSystem FilePath Text, Output Log] r

runControllerC :: CompileOpts 
               -> Sem '[Error CompilationError, Reader CompileOpts, FileSystem FilePath Text, Output Log, Fresh (Text, LexInfo) QualifiedName, Embed IO] a
               -> IO ([Log], Either CompilationError a)
runControllerC opts r = runM (runFreshQNamesState $ outputToIOMonoidAssocR pure $ fileSystemIO $ runReader opts $ runError r)
        `Ex.catch` (\(Ex.SomeException e) -> pure ([], Left (Panic (show e))))

data CompileOpts = CompileOpts {
      name::Text
    , debug::Bool
    , description::Text
    , ddumpAsm::Bool
    , ddumpLC::Bool
    , ddumpCPS::Bool
    , ddumpReduced::Bool
    , ddumpTL::Bool
    }

askDataPackOpts :: (Member (Reader CompileOpts) r) => Sem r DataPackOptions
askDataPackOpts = ask <&> \(CompileOpts{name, description}) -> DataPackOptions {
        name
    ,   description
    }

compileToDataPack :: (ControllerC r, Members '[Time, Fresh (Text, LexInfo) QualifiedName] r) => [FilePath] -> Sem r LByteString
compileToDataPack files = do
    cmods <- compileAll files
    opts <- askDataPackOpts
    makeDataPack opts cmods

compileContentsToDataPack :: (ControllerC r, Members '[Time, Fresh (Text, LexInfo) QualifiedName] r) => [(FilePath, Text)] -> Sem r LByteString
compileContentsToDataPack files = do
    cmods <- compileContents files
    opts <- askDataPackOpts
    makeDataPack opts cmods

compileAll :: (ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r) => [FilePath] -> Sem r [CompiledModule]
compileAll files = compileContents =<< traverse (\x -> (x,) <$> readFile x) files

compileContents :: (ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r) => [(FilePath, Text)] -> Sem r [CompiledModule]
compileContents contents = do
    tokens <- traverse (\(fn, content) -> mapError LexError $ tokenize (toText fn) content) contents
    asts   <- traverse (\(ts, n) -> mapError ParseError $ fromEither $ parse (module_ (getModName n)) n ts) (zip tokens (map fst contents))

    orderedMods :: [(S.Module 'SolveModules, [Text])] <- mapError ModuleError $ findCompilationOrder asts

    fmap join $ evalState (one ("prims", primModSig)) $ traverse compileAndAnnotateSig orderedMods

compileAndAnnotateSig :: (ControllerC r, Members '[State (Map (S.Name 'QualifyNames) ModSig), Fresh (Text, LexInfo) QualifiedName] r)
                      => (S.Module 'SolveModules, [S.Name 'SolveModules])
                      -> Sem r [CompiledModule]
compileAndAnnotateSig (m, deps) = do
    annotatedMod :: (S.Module 'QualifyNames) <- S.Module
        <$> Ext . fromList <$> traverse (\d -> (internalQName d ,) <$> getDep d) ("prims" : deps)
        <*> pure (S.moduleName m)
        <*> pure (map (coercePass @(Statement SolveModules) @(Statement QualifyNames) @SolveModules @QualifyNames) (moduleStatements m))
    (compMod, sig) <- compileWithSig annotatedMod
    modify (insert (S.moduleName m) sig)
    pure compMod


getDep :: (ControllerC r, Member (State (Map (S.Name 'QualifyNames) ModSig)) r)
       => S.Name 'SolveModules
       -> Sem r ModSig
getDep n = maybe (error $ "Module dependency '" <> show n <> "' not found") pure =<< gets (lookup n)
--              TODO^: Should this really be a panic? 
compileWithSig :: (ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r)
               => S.Module 'QualifyNames
               -> Sem r ([CompiledModule], ModSig)
compileWithSig m = do
    let qualScopes = [Scope {
            _scopeVars = mempty
        ,   _scopeTypes = mempty
        ,   _scopeFixities = mempty
        ,   _scopeTVars = mempty
        }]
    let tcState = foldMap (\dsig -> TCState {
                    varTypes=exportedVars dsig
                })
                (getExt $ xModule m)
    --compEnv <- asks \CompileOpts{name, debug, target} -> CompEnv {nameSpace=name, debug, A.target=target}

    qMod  <- mapError QualificationError $ runReader qualScopes $ qualify m

    saMod <- mapError SemanticError $ runSemanticAnalysis qMod

    tcMod <- mapError TypeError $ evalState tcState $ runOutputSem (log LogWarning . displayTWarning) $ typecheckModule saMod

    compMods <- freshWithInternal do
        let lc  = C2LC.compile primOps tcMod
        whenM (asks ddumpLC) $ dumpLC lc

        cps     <- LC2CPS.compile lc
        whenM (asks ddumpCPS) $ dumpCPS cps

        let reduced = LC2CPS.reduceAdmin cps
        whenM (asks ddumpReduced) $ dumpReduced reduced

        tl      <- CPS2TL.compile reduced
        whenM (asks ddumpTL) $ dumpTL tl
        
        let asm = TL2ASM.compile tl 
        whenM (asks ddumpAsm) $ dumpAsm asm
        
        pure $ ASM2MC.compile asm
    
    let sig = extractSig tcMod

    pure (compMods, sig)

displayTWarning :: TypeWarning -> Text
displayTWarning = show

extractSig :: S.Module 'Codegen -> ModSig
extractSig (S.Module _deps _n sts) = foldMap makePartialSig sts

makePartialSig :: S.Statement 'Codegen -> ModSig
makePartialSig = \case
    Def _ _ (Decl _ n _ _) t        -> mempty {exportedVars = one (n, t)}
    DefStruct (Ext k) _ n _ps fs    -> mempty {exportedTypes = one (n, (k, RecordType fs))}
    Import IgnoreExt _ _            -> mempty
    StatementX v _                  -> absurd v

getModName :: FilePath -> Text
getModName = toText . FP.dropExtension . L.last . segments


primModSig :: ModSig
primModSig = ModSig {
        exportedVars = fmap (view primOpType) $ primOps
            -- The type application is only necessary to satisfy the type checker since the value depending on the type 'r' is ignored
    ,   exportedTypes = fromList [
              (internalQName "Int", (KStar, BuiltInType))
            , (internalQName "Bool", (KStar, BuiltInType))
            , (internalQName "Entity", (KStar, BuiltInType))
            , (internalQName "Unit", (KStar, BuiltInType))
            , (internalQName "->", (KStar `KFun` KStar `KFun` KStar, BuiltInType))
            ]
    ,   exportedFixities = mempty
    }

dumpLC :: (Members '[FileSystem FilePath Text] r) => LCExpr -> Sem r ()
dumpLC = writeFile "dump-lc.lc" . prettyPrintLCExpr

dumpCPS :: (Members '[FileSystem FilePath Text] r) => CPS -> Sem r ()
dumpCPS = writeFile "dump-cps.lc" . show

dumpReduced :: (Members '[FileSystem FilePath Text] r) => CPS -> Sem r ()
dumpReduced = writeFile "dump-reduced-cps.lc" . show

dumpTL :: (Members '[FileSystem FilePath Text] r) => TL -> Sem r ()
dumpTL = writeFile "dump-tl.lc" . show 

dumpAsm :: (Members '[FileSystem FilePath Text] r) => [Block] -> Sem r ()
dumpAsm = writeFile "dump-asm.mcasm" . renderAsm
    where
        renderAsm :: [Block] -> Text
        renderAsm = T.intercalate "\n\n" . map (\(Block f is) -> "[" <> show f <> "]:\n" <> foldMap (\i -> "    " <> show i <> "\n") is)



