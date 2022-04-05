module Language.Cobble (
      compileAll
    , compileContents
    , compileToRacketFile
    , runControllerC
    , CompilationError(..)
    , CompileOpts(..)
    , Target(..)
    , ControllerC

    , module Language.Cobble.Util.Trace

    , primModSig
    , modSigToScope
    ) where

import Language.Cobble.Prelude hiding ((<.>), readFile, writeFile, combine)

import Language.Cobble.Util.Trace

import Language.Cobble.Types as S
import Language.Cobble.Parser.Tokenizer as S
import Language.Cobble.Parser as S
import Language.Cobble.Qualifier as S
import Language.Cobble.SemAnalysis as S
import Language.Cobble.ModuleSolver
import Language.Cobble.Util.Polysemy.Time
import Language.Cobble.Util.Polysemy.FileSystem
import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Util.Polysemy.Dump
import Language.Cobble.Util.Polysemy.StackState

import Language.Cobble.Prelude.Parser (ParseError, parse)

import Language.Cobble.Typechecker as TC

import Language.Cobble.Codegen.PrimOp

import Language.Cobble.Core.Lower as Lower
import Language.Cobble.Core.Lint
import Language.Cobble.Codegen.CoreToRacket as CoreToRacket

import Language.Cobble.Racket.Types as Racket
import Language.Cobble.Core.Types qualified as Core

import Data.Map qualified as M

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


type ControllerC r = Members '[Reader CompileOpts, Error CompilationError, FileSystem FilePath Text] r


runControllerC :: CompileOpts 
               -> Sem '[Error CompilationError, Reader CompileOpts, FileSystem FilePath Text, Fresh (Text, LexInfo) QualifiedName, Embed IO] a
               -> IO (Either CompilationError a)
runControllerC opts r = runM (runFreshQNamesState $ fileSystemIO $ runReader opts $ runError r)
        `Ex.catch` (\(Ex.SomeException e) -> pure (Left (Panic (show e))))

data CompileOpts = CompileOpts {
      name::Text
    , debug::Bool
    , description::Text
    , target::Target
    , ddumpTC::Bool
    , ddumpCore::Bool
    , skipCoreLint::Bool
    }

data Target = Racket
            deriving (Show, Eq)
instance R.Read Target where
    readsPrec _ "racket" = [(Racket, "")]
    readsPrec _ _ = []

class Compiled m where
    compileFromCore :: forall r. (ControllerC r, Members '[Fresh Text QualifiedName] r) => Seq Core.Decl -> Sem r m

instance Compiled (Seq RacketExpr) where
    compileFromCore = CoreToRacket.compile

compileToRacketFile :: (Trace, ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r) => Seq FilePath -> Sem r Text
compileToRacketFile files = show . prettyRacketWithRuntime <$> compileAll files

compileAll :: (Trace, ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r, Compiled m) => Seq FilePath -> Sem r m
compileAll files = compileContents =<< traverse (\x -> (x,) <$> readFile x) files

compileContents :: (Trace, ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r, Compiled m) 
                => Seq (FilePath, Text)
                -> Sem r m
compileContents contents = do
    tokens <- traverse (\(fn, content) -> mapError LexError $ tokenize (toText fn) content) contents
    asts   <- traverse (\(ts, n) -> mapError ParseError $ fromEither $ parse module_ n ts) (zip tokens (map fst contents))

    orderedMods :: Seq (S.Module 'SolveModules, Seq Text) <- mapError ModuleError $ findCompilationOrder asts

    core <- fmap join $ evalState (one ("prims", primModSig)) $ traverse compileAndAnnotateSig orderedMods
    
    whenM (asks (not . skipCoreLint)) do
        lintError <- runError $ lint (LintEnv mempty mempty mempty) core

        case lintError of
            Left (MkCoreLintError msg) -> traceM Warning $ "[CORE LINT ERROR]: " <> msg
            Right () -> pure () 

    freshWithInternal $ compileFromCore core

compileAndAnnotateSig :: (Trace, ControllerC r, Members '[State (Map (S.Name 'QualifyNames) ModSig), Fresh (Text, LexInfo) QualifiedName] r)
                      => (S.Module 'SolveModules, Seq (S.Name 'SolveModules))
                      -> Sem r (Seq Core.Decl)
compileAndAnnotateSig (m, deps) = do
    annotatedMod :: (S.Module 'QualifyNames) <- S.Module
        <$> fromList . toList <$> traverse (\d -> (internalQName d ,) <$> getDep d) ("prims" <| deps)
        <*> pure (S.moduleName m)
        <*> pure (map (coercePass @(Statement SolveModules) @(Statement QualifyNames)) (moduleStatements m))
    (core, sig) <- compileWithSig annotatedMod
    modify (insert (S.moduleName m) sig)
    pure core

getDep :: (ControllerC r, Member (State (Map (S.Name 'QualifyNames) ModSig)) r)
       => S.Name 'SolveModules
       -> Sem r ModSig
getDep n = maybe (error $ "Module dependency '" <> show n <> "' not found") pure =<< gets (lookup n)
--              TODO^: Should this really be a panic? 
compileWithSig :: (Trace, ControllerC r, Members '[Fresh (Text, LexInfo) QualifiedName] r)
               => S.Module 'QualifyNames
               -> Sem r (Seq Core.Decl, ModSig)
compileWithSig m = do
    let qualScopes = map modSigToScope (fromList $ toList $ xModule m)

    let tcEnv = foldr (\dsig r -> TCEnv {
                    _varTypes = fmap coercePass (exportedVars dsig <> (view _1 <$> exportedVariantConstrs dsig)) <> _varTypes r
                ,   _tcInstances = coercePass (exportedInstances dsig) <> _tcInstances r
                })
                (TCEnv mempty mempty)
                (toList $ xModule m)
    qMod  <- mapError QualificationError $ evalStackStatePanic (fold qualScopes) $ qualify m

    saMod <- mapError SemanticError $ runSemanticAnalysis qMod

    freshWithInternal do
        tcMod <- -- dumpWhenWithM (asks ddumpTC) ppGivens "dump-givens.tc" 
            -- $ dumpWhenWithM (asks ddumpTC) ppWanteds "dump-wanteds.tc" 
            dumpWhenWithM (asks ddumpTC) ppTC "dump-tc" 
            $ mapError TypeError 
            $ runFreshM (\(MkTVar n k) -> freshVar (originalName n) <&> \n' -> MkTVar n' k)
            $ typecheck tcEnv saMod -- TODO: provide environment from other modules

        core <- lower tcMod

        dumpWhenWithM (asks ddumpCore) (show . Core.prettyDecls) "dump-core" $ dump core

        pure (core, extractSig tcMod)


modSigToScope :: ModSig -> Scope
modSigToScope (ModSig{exportedVars, exportedVariantConstrs, exportedTypes, exportedFixities}) = Scope {
                _scopeVars              = fromList $ fmap (\(qn, _) -> (originalName qn, qn)) $ M.toList exportedVars
            ,   _scopeVariantConstrs    = fromList $ fmap (\(qn, (_, ep, i, v)) -> (originalName qn, (qn, ep, i, v))) $ M.toList exportedVariantConstrs
            ,   _scopeTypes             = fromList $ fmap (\(qn, (k, tv)) -> (originalName qn, (qn, k, tv, True))) $ M.toList exportedTypes
            ,   _scopeFixities          = M.mapKeys originalName exportedFixities
            ,   _scopeTVars             = mempty
            }

extractSig :: S.Module 'Codegen -> ModSig
extractSig (S.Module _deps _n sts) = foldMap makePartialSig sts

makePartialSig :: S.Statement 'Codegen -> ModSig
makePartialSig = \case
    Def mfixity _ (Decl (_, gs) n _ _) t -> mempty {exportedVars = one (n, t), exportedFixities = fromList $ (n,) <$> toList mfixity} -- TODO: what about gs?
    DefClass k _ n ps meths   -> mempty 
        {   exportedTypes = one (n, (k, TyClass ps meths))
        ,   exportedVars  = fromList $ 
                toList $ map (second coercePass) meths
        }
    DefInstance (_, _, _, dictName) _ cname ty _ -> mempty {exportedInstances = one (cname, [(ty, dictName)])}
    DefVariant k _ tyName ps cs -> let tyVariant = VariantType ps (map (\(x,y,_) -> (x,y)) cs) in 
        mempty
        {   exportedTypes = one (tyName, (k, tyVariant))
        ,   exportedVariantConstrs = fromList $ toList (map (\(cname, _, (ty, ep, i)) -> (cname, (ty, ep, i, tyVariant))) cs)
        }
    Import () _ _            -> mempty


primModSig :: ModSig
primModSig = ModSig {
        exportedVars = fmap primOpType $ primOps
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

