module Cobble.ModuleSolver where

import Cobble.Prelude

import Cobble.Types
import Cobble.Interface

data ModuleError = ModuleNotFound (Name 'SolveModules)
                 | InvalidImportPosition LexInfo Text
                 deriving (Show, Eq)

type NextPass = 'QualifyNames

insertInterfaceSigs :: Members '[Error ModuleError] r => Seq Interface -> Module SolveModules -> Sem r (Module NextPass)
insertInterfaceSigs ifaces (Module () mname mstmnts) = do
    (imports, mstmnts') <- collectImports mstmnts
    imports' <- fromList . toList <$> forM imports \imp -> case lookup imp ifaceMap of
        Nothing -> throw $ ModuleNotFound imp
        Just mod -> pure (imp, mod)
    pure (Module imports' mname mstmnts')
        where
            ifaceMap :: Map Text ModSig
            ifaceMap = fromList $ toList (map (\Interface{interfaceModName, interfaceModSig} -> (interfaceModName, interfaceModSig)) ifaces)

collectImports :: Members '[Error ModuleError] r 
               => Seq (Statement SolveModules) 
               -> Sem r (Seq Text, Seq (Statement NextPass))
collectImports (Import () li mod :<| sts) = first (mod :<|) <$> collectImports sts
collectImports sts = ([],) <$> traverse ensureNoExcessiveImport sts

ensureNoExcessiveImport :: Members '[Error ModuleError] r 
                         => Statement SolveModules
                         -> Sem r (Statement NextPass)
ensureNoExcessiveImport (Import () li mod) = throw $ InvalidImportPosition li mod
ensureNoExcessiveImport (Def ext li decl ty) = pure $ Def ext li (coercePass decl) ty
ensureNoExcessiveImport (DefClass ext li cname tvs clauses) = pure $ DefClass ext li cname tvs clauses
ensureNoExcessiveImport (DefInstance ext li cname ty decls) = pure $ DefInstance ext li cname ty (coercePass decls)
ensureNoExcessiveImport (DefVariant ext li cname tvs clauses) = pure $ DefVariant ext li cname tvs clauses
