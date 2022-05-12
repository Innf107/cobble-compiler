module Cobble (
      compileAll
    , compileContents
    , compileToRacketFile
    , runControllerC
    , CompilationError(..)
    , CompileOpts(..)
    , Target(..)
    , ControllerC

    , module Cobble.Util.Trace

    , primModSig
    , modSigToScope
    ) where

import Cobble.Prelude hiding ((<.>), readFile, writeFile, combine)

import Cobble.Util.Trace

import Cobble.Types as S
import Cobble.Parser.Tokenizer as S
import Cobble.Parser as S
import Cobble.Qualifier as S
import Cobble.SemAnalysis as S
import Cobble.ModuleSolver
import Cobble.Util.Polysemy.Time
import Cobble.Util.Polysemy.FileSystem
import Cobble.Util.Polysemy.Fresh
import Cobble.Util.Polysemy.Dump
import Cobble.Util.Polysemy.StackState
import Cobble.Util.Polysemy.Context

import Cobble.Prelude.Parser (ParseError, parse)

import Cobble.Typechecker as TC

import Cobble.Codegen.PrimOp

import Cobble.Core.Lower as Lower
import Cobble.Core.Lint
import Cobble.Codegen.CoreToRacket as CoreToRacket

import Cobble.Racket.Types as Racket
import Cobble.Core.Types qualified as Core

import Cobble.Interface

import Data.Map qualified as M

import Data.Binary

import qualified Control.Exception as Ex

import qualified GHC.Read as R

data CompilationError = LexError LexicalError
                      | ParseError ParseError
                      | QualificationError QualificationError
                      | SemanticError SemanticError
                      | TypeError TypeError
                      | ModuleError ModuleError
                      | Panic Text
                      | InterfaceDecodeError Text FilePath
                      deriving (Show, Eq)


type ControllerC r = Members '[Reader CompileOpts, Error CompilationError, FileSystem FilePath Text] r


runControllerC :: CompileOpts 
               -> Sem '[Error CompilationError, Reader CompileOpts, FileSystem FilePath LByteString, FileSystem FilePath Text, Fresh Text QualifiedName, Embed IO] a
               -> IO (Either CompilationError a)
runControllerC opts r = handle
                      $ runM 
                      $ runFreshQNamesState 
                      $ runFileSystemGenericStringIO
                      $ runFileSystemLByteStringIO 
                      $ runReader opts 
                      $ runError r
    where
        handle m = m `Ex.catch` (\(Ex.SomeException e) -> pure (Left (Panic (show e))))

data CompileOpts = CompileOpts {
      interfaceFiles :: Seq FilePath
    , target :: Target
    , ddumpRenamed :: Bool
    , ddumpPostProcessed :: Bool
    , ddumpTC :: Bool
    , ddumpCore :: Bool
    , skipCoreLint :: Bool
    }

data Target = Racket
            deriving (Show, Eq)
instance R.Read Target where
    readsPrec _ "racket" = [(Racket, "")]
    readsPrec _ _ = []

class Compiled m where
    compileFromCore :: forall r. (ControllerC r, Members '[Fresh Text QualifiedName] r) 
                    => Seq Interface 
                    -> Seq Core.Decl 
                    -> Sem r m

instance Compiled (Seq RacketExpr) where
    compileFromCore = CoreToRacket.compile

compileToRacketFile :: (Trace, ControllerC r, Members '[Fresh Text QualifiedName, FileSystem FilePath LByteString] r) 
                    => FilePath 
                    -> Sem r (Text, Interface)
compileToRacketFile files = bimap (show . prettyRacketExprs) id <$> compileAll files

compileAll :: (Trace, ControllerC r, Members '[Fresh Text QualifiedName, FileSystem FilePath LByteString] r, Compiled m) 
           => FilePath 
           -> Sem r (m, Interface)
compileAll file = compileContents file =<< readFile file

compileContents :: (Trace, ControllerC r, Members '[Fresh Text QualifiedName, FileSystem FilePath LByteString] r, Compiled m) 
                => FilePath
                -> Text
                -> Sem r (m, Interface)
compileContents path content = do
    interfaces <- traverse readInterfaceFile =<< asks interfaceFiles

    tokens <- mapError LexError $ tokenize (toText path) content
    ast    <- mapError ParseError $ fromEither $ parse module_ path tokens

    modWithSigs <- mapError ModuleError $ insertInterfaceSigs interfaces ast

    (core, sig) <- compileWithSig modWithSigs
    
    whenM (asks (not . skipCoreLint)) do
        
        let (CoreModSig{coreModVars, coreModDictTyDefs, coreModEffs}) = foldr (mergeCoreModSig . interfaceCoreModSig) primCoreSig interfaces
        
        let lintEnv = LintEnv {
                varTypes = coreModVars
            ,   jpArgTypes = mempty
            ,   dictTyDefs = coreModDictTyDefs
            ,   effDefs = coreModEffs
            ,   opEffs = fromList $ toList $ foldMap (\(effName, (_, ops)) -> map (\(op, _) -> (op, effName)) ops) (M.toList coreModEffs)
            }

        lintError <- runError $ lint lintEnv core

        case lintError of
            Left (MkCoreLintError msg) -> traceM TraceCoreLint $ msg
            Right () -> pure () 

    result <- compileFromCore interfaces core

    let (Module _ moduleName _) = modWithSigs
    let interface = Interface {
            interfaceModName = moduleName
        ,   interfaceModSig = sig
        ,   interfaceCoreModSig = extractCoreSig core
        }

    pure (result, interface)

compileWithSig :: (Trace, ControllerC r, Members '[Fresh Text QualifiedName] r)
               => S.Module 'QualifyNames
               -> Sem r (Seq Core.Decl, ModSig)
compileWithSig m = do
    let modSigs = primModSig :<| fromList (toList (xModule m))

    let qualScopes = map modSigToScope modSigs

    let tcEnv = foldr (\dsig r -> TCEnv {
                    _varTypes = fmap coercePass (exportedVars dsig <> (view _1 <$> exportedVariantConstrs dsig)) <> _varTypes r
                ,   _tcInstances = coercePass (exportedInstances dsig) <> _tcInstances r
                })
                (TCEnv mempty mempty)
                modSigs
    qMod  <- mapError QualificationError $ evalStackStatePanic (fold qualScopes) $ qualify m

    dumpWhenWithM (asks ddumpRenamed) (show . pretty) "dump-renamed" $ dump qMod

    saMod <- mapError SemanticError $ runSemanticAnalysis qMod
    
    dumpWhenWithM (asks ddumpPostProcessed) (show . pretty) "dump-postprocessed" $ dump saMod

    tcMod <- dumpWhenWithM (asks ddumpTC) ppTC "dump-tc" 
           $ mapError TypeError 
           $ runContext
           $ runFreshM (\(MkTVar n k) -> freshVar (originalName n) <&> \n' -> MkTVar n' k)
           $ typecheck tcEnv saMod -- TODO: provide environment from other modules

    core <- lower tcMod

    dumpWhenWithM (asks ddumpCore) (show . Core.prettyDecls) "dump-core" $ dump core

    pure (core, extractSig tcMod)


modSigToScope :: ModSig -> Scope
modSigToScope (ModSig{exportedVars, exportedVariantConstrs, exportedTypes, exportedFixities, exportedEffOperations}) = Scope {
                _scopeVars              = fromList $ fmap (\(qn, _) -> (originalName qn, qn)) $ M.toList exportedVars
            ,   _scopeVariantConstrs    = fromList $ fmap (\(qn, (_, ep, i, v)) -> (originalName qn, (qn, ep, i, v))) $ M.toList exportedVariantConstrs
            ,   _scopeTypes             = fromList $ fmap (\(qn, (k, tv)) -> (originalName qn, (qn, k, tv, True))) $ M.toList exportedTypes
            ,   _scopeFixities          = M.mapKeys originalName exportedFixities
            ,   _scopeTVars             = mempty
            ,   _scopeEffOperations     = fromList $ fmap (\(qn, _) -> (originalName qn, qn)) $ M.toList exportedEffOperations 
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
    DefEffect k _ effName ps ops -> let tyVariant = TyEffect ps ops in
        mempty 
        {   exportedTypes = one (effName, (k, tyVariant))
        ,   exportedEffOperations = fromList $ toList ops
        }


primCoreSig :: CoreModSig
primCoreSig = CoreModSig {
    coreModVars = fmap (run . lowerType . primOpType) $ primOps
,   coreModDictTyDefs = mempty
,   coreModEffs = mempty
}

primModSig :: ModSig
primModSig = ModSig {
        exportedVars = fmap primOpType $ primOps
    ,   exportedVariantConstrs = mempty
    ,   exportedTypes = fromList [
              (internalQName "Int", (KStar, BuiltInType))
            ]
    ,   exportedFixities = mempty
    ,   exportedInstances = mempty
    ,   exportedEffOperations = mempty
    }

readInterfaceFile :: Members '[FileSystem FilePath LByteString, Error CompilationError] r => FilePath -> Sem r Interface
readInterfaceFile filePath = do
    contents <- readFile filePath
    case decodeOrFail contents of
        Left (_, _, message) -> throw (InterfaceDecodeError (toText message) filePath)
        Right (_, _, iface) -> pure iface
