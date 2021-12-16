module Language.Cobble (
      compileAll
    , compileContents
    , compileToDataPack
    , compileContentsToDataPack
    , compileToLuaFile
    , runControllerC
    , LogLevel(..)
    , Log(..)
    , CompilationError(..)
    , CompileOpts(..)
    , Target(..)
    , ControllerC


    , primModSig
    , modSigToScope
    ) where

import Language.Cobble.Prelude hiding ((<.>), readFile, writeFile, combine)

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
import Language.Cobble.Util.Polysemy.Dump
import Language.Cobble.Util.Polysemy.StackState
import Language.Cobble.Util

import Language.Cobble.Prelude.Parser (ParseError, parse)

import Language.Cobble.Typechecker as TC

import Language.Cobble.PostProcess as PP

import Language.Cobble.MCAsm.Types
import Language.Cobble.MCAsm.Types qualified as A

import Language.Cobble.McFunction.Types

import Language.Cobble.Lua.Types as Lua
import Language.Cobble.Lua.PrettyPrint as Lua

import Language.Cobble.Codegen.PrimOps

import Language.Cobble.Codegen.CobbleToLC as C2LC
import Language.Cobble.Codegen.LCToBasicCPS as LC2CPS
import Language.Cobble.Codegen.BasicCPSToTopLevelCPS as CPS2TL
import Language.Cobble.Codegen.TopLevelCPSToMCAsm as TL2ASM
import Language.Cobble.Codegen.MCAsmToMCFunction as ASM2MC

import Language.Cobble.Codegen.CPSToLua as CPS2Lua

import Language.Cobble.LC.Types as LC
import Language.Cobble.LC.PrettyPrint as LC

import Language.Cobble.CPS.Basic.Types
import Language.Cobble.CPS.TopLevel.Types

import Data.Map qualified as M
import Data.List qualified as L
import Data.Text qualified as T
import System.FilePath qualified as FP

import qualified Control.Exception as Ex

import qualified GHC.Read as R

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
    , target::Target
    , ddumpTC::Bool
    , ddumpAsm::Bool
    , ddumpLC::Bool
    , ddumpCPS::Bool
    , ddumpReduced::Bool
    , ddumpTL::Bool
    }

data Target = MC117
            | Lua
            deriving (Show, Eq)
instance R.Read Target where
    readsPrec _ "mc-1.17" = [(MC117, "")]
    readsPrec _ "lua" = [(Lua, "")] 
    readsPrec _ _ = []

askDataPackOpts :: (Member (Reader CompileOpts) r) => Sem r DataPackOptions
askDataPackOpts = ask <&> \(CompileOpts{name, description}) -> DataPackOptions {
        name
    ,   description
    }

class Compiled m where
    compileFromLC :: forall r. (ControllerC r, Members '[Fresh Text QualifiedName] r) => LCExpr -> Sem r m

instance Compiled [CompiledModule] where
    compileFromLC lc = do
        cps     <- LC2CPS.compile lc
        whenM (asks ddumpCPS) $ dumpCPS cps

        let reduced = LC2CPS.reduceAdmin cps
        whenM (asks ddumpReduced) $ dumpReduced reduced

        tl      <- CPS2TL.compile reduced
        whenM (asks ddumpTL) $ dumpTL tl
        
        let asm = TL2ASM.compile tl 
        whenM (asks ddumpAsm) $ dumpAsm asm
        
        pure $ ASM2MC.compile asm

instance Compiled [LuaStmnt] where
    compileFromLC lc = do
        cps <- LC2CPS.compile lc
        whenM (asks ddumpCPS) $ dumpCPS cps
        
        let reduced = LC2CPS.reduceAdmin cps
        whenM (asks ddumpReduced) $ dumpReduced reduced

        pure (CPS2Lua.compile reduced)

compileToLuaFile :: (ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r) => [FilePath] -> Sem r Text
compileToLuaFile files = prettyLua <$> compileAll files

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

compileAll :: (ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r, Compiled m) => [FilePath] -> Sem r m
compileAll files = compileContents =<< traverse (\x -> (x,) <$> readFile x) files

compileContents :: (ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r, Compiled m) 
                => [(FilePath, Text)] 
                -> Sem r m
compileContents contents = do
    tokens <- traverse (\(fn, content) -> mapError LexError $ tokenize (toText fn) content) contents
    asts   <- traverse (\(ts, n) -> mapError ParseError $ fromEither $ parse (module_ (getModName n)) n ts) (zip tokens (map fst contents))

    orderedMods :: [(S.Module 'SolveModules, [Text])] <- mapError ModuleError $ findCompilationOrder asts

    lcDefs <- fmap join $ evalState (one ("prims", primModSig)) $ traverse compileAndAnnotateSig orderedMods
    
    let lc = C2LC.collapseDefs lcDefs

    whenM (asks ddumpLC) $ dumpLC lc

    freshWithInternal $ compileFromLC lc

compileAndAnnotateSig :: (ControllerC r, Members '[State (Map (S.Name 'QualifyNames) ModSig), Fresh (Text, LexInfo) QualifiedName] r)
                      => (S.Module 'SolveModules, [S.Name 'SolveModules])
                      -> Sem r [LCDef]
compileAndAnnotateSig (m, deps) = do
    annotatedMod :: (S.Module 'QualifyNames) <- S.Module
        <$> fromList <$> traverse (\d -> (internalQName d ,) <$> getDep d) ("prims" : deps)
        <*> pure (S.moduleName m)
        <*> pure (map (coercePass @(Statement SolveModules) @(Statement QualifyNames)) (moduleStatements m))
    (lcdefs, sig) <- compileWithSig annotatedMod
    modify (insert (S.moduleName m) sig)
    pure lcdefs

getDep :: (ControllerC r, Member (State (Map (S.Name 'QualifyNames) ModSig)) r)
       => S.Name 'SolveModules
       -> Sem r ModSig
getDep n = maybe (error $ "Module dependency '" <> show n <> "' not found") pure =<< gets (lookup n)
--              TODO^: Should this really be a panic? 
compileWithSig :: (ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r)
               => S.Module 'QualifyNames
               -> Sem r ([LCDef], ModSig)
compileWithSig m = do
    let qualScopes = Scope {
            _scopeVars = mempty
        ,   _scopeVariantConstrs = mempty
        ,   _scopeTypes = mempty
        ,   _scopeFixities = mempty
        ,   _scopeTVars = mempty
        } : map modSigToScope (toList $ xModule m)

    let tcState = foldMap (\dsig -> TCState {
                    _varTypes= fmap coercePass $ exportedVars dsig
                ,   _tcInstances = coercePass $ exportedInstances dsig
                })
                (xModule m)
    qMod  <- mapError QualificationError $ evalStackStatePanic (mconcat qualScopes) $ qualify m

    saMod <- mapError SemanticError $ runSemanticAnalysis qMod

    freshWithInternal do
        tcMod <- dumpWhenWithM (asks ddumpTC) ppGivens "dump-givens.tc" 
            $ dumpWhenWithM (asks ddumpTC) ppWanteds "dump-wanteds.tc" 
            $ dumpWhenWithM (asks ddumpTC) ppTC "dump-tc.tc" 
            $ mapError TypeError $ evalState tcState $ typecheck saMod

        let ppMod = postProcess tcMod

        lc <- C2LC.compile primOps ppMod

        pure (lc, extractSig ppMod)


modSigToScope :: ModSig -> Scope
modSigToScope (ModSig{exportedVars, exportedVariantConstrs, exportedTypes, exportedFixities}) = Scope {
                _scopeVars              = fromList $ map (\(qn, _) -> (originalName qn, qn)) $ M.toList exportedVars
            ,   _scopeVariantConstrs    = fromList $ map (\(qn, (_, ep, i)) -> (originalName qn, (qn, ep, i))) $ M.toList exportedVariantConstrs
            ,   _scopeTypes             = fromList $ map (\(qn, (k, tv)) -> (originalName qn, (qn, k, tv))) $ M.toList exportedTypes
            ,   _scopeFixities          = M.mapKeys originalName exportedFixities
            ,   _scopeTVars             = mempty
            }

extractSig :: S.Module 'Codegen -> ModSig
extractSig (S.Module _deps _n sts) = foldMap makePartialSig sts

makePartialSig :: S.Statement 'Codegen -> ModSig
makePartialSig = \case
    Def _ _ (Decl (_, gs) n _ _) t        -> mempty {exportedVars = one (n, t)} -- TODO: what about gs?
    DefStruct k _ n ps fs     -> mempty {exportedTypes = one (n, (k, RecordType ps fs))}
    DefClass k _ n ps meths   -> mempty 
        {   exportedTypes = one (n, (k, TyClass ps meths))
        ,   exportedVars  = fromList $ 
                map (second coercePass) meths
        }
    DefInstance _ _ cname ty _ -> mempty {exportedInstances = one (cname, [ty])}
    DefVariant k _ tyName ps cs    -> mempty
        {   exportedTypes = one (tyName, (k, VariantType ps (map (\(x,y,_) -> (x,y)) cs)))
        ,   exportedVariantConstrs = fromList (map (\(cname, _, (ty, ep, i)) -> (cname, (ty, ep, i))) cs)
        }
    Import () _ _            -> mempty

getModName :: FilePath -> Text
getModName = toText . FP.dropExtension . L.last . segments


primModSig :: ModSig
primModSig = ModSig {
        exportedVars = fmap (view primOpType) $ primOps
    ,   exportedVariantConstrs = mempty
            -- The type application is only necessary to satisfy the type checker since the value depending on the type 'r' is ignored
    ,   exportedTypes = fromList [
              (internalQName "Int", (KStar, BuiltInType))
            , (internalQName "Bool", (KStar, BuiltInType))
            , (internalQName "Entity", (KStar, BuiltInType))
            , (internalQName "Unit", (KStar, BuiltInType))
            , (internalQName "->", (KStar `KFun` KStar `KFun` KStar, BuiltInType))
            ]
    ,   exportedFixities = mempty
    ,   exportedInstances = mempty
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

