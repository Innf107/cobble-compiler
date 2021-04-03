module Language.Cobble.ModuleSolver where

import Language.Cobble.Prelude

import Language.Cobble.Types

type ModC (r :: EffectRow) = Members '[Error ModuleError] r

data ModuleError = CircularDependency [[Name 'SolveModules]]
                 | ModuleDoesNotExist (Name 'SolveModules)
                 deriving (Show, Eq)

data DepModule = DepModule {
        modName :: Name 'SolveModules
    ,   modDeps :: [DepModule]
    } deriving (Show, Eq)

data ModSig = ModSig {
    sigTypes :: Map QualifiedName (Type 'Codegen)
}

type NextPass = 'QualifyNames

-- | Finds an order for a list of modules such that
-- whenever a module is compiled, all its dependencies have already been compiled
-- (a topological sort).
--
-- This function is not very efficient, but that should
-- not matter even if there are a lot of modules in a project.
-- In case it does matter, the function should be fairly easy to optimize.
solveModuleOrder :: (ModC r) => [(DepModule, a)] -> Sem r [a]
solveModuleOrder [] = pure []
solveModuleOrder ms = case find (null . modDeps . fst) ms of
    Nothing -> findCircularDeps (map fst ms)
    Just (m, x) -> (x:) <$> solveModuleOrder 
        (map (first (\dm -> dm{modDeps = removeMod (modDeps dm)})) $ filter ((/=modName m) . modName . fst) ms)
        where
            removeMod :: [DepModule] -> [DepModule]
            removeMod = filter ((/=modName m) . modName)
 
findCircularDeps :: (ModC r) => [DepModule] -> Sem r a
findCircularDeps ms = throw $ CircularDependency $ catMaybes $ map (flip getCycle []) ms
    where
        getCycle :: DepModule -> [DepModule] -> Maybe [Name 'SolveModules]
        getCycle (DepModule _ []) _ = Nothing
        getCycle m@(DepModule n ds) soFar
            | any ((==n) . modName) soFar = Just (dropWhile (/=n) $ map modName (m:soFar))
            | otherwise = concat <$> traverse (flip getCycle (m:soFar)) ds


-- TODO!: Detect cycles here
toDepMod :: forall r. (ModC r) => [Module 'SolveModules] -> Module 'SolveModules -> Sem r DepModule
toDepMod ms m@(Module () mname _) = fmap (DepModule mname) $ getModDeps m
    where
        getModDeps :: Module 'SolveModules -> Sem r [DepModule]
        getModDeps (Module () _ msts) = msts & mapMaybeM \case
            Import () _ importedMName -> case find ((==importedMName) . moduleName) ms of
                Nothing -> throw $ ModuleDoesNotExist importedMName
                Just importedMod -> Just . DepModule importedMName <$> getModDeps importedMod
            _ -> pure Nothing

determineDeps :: (ModC r) => [Module 'SolveModules] -> Sem r [DepModule]
determineDeps ms = traverse (toDepMod ms) ms

findCompilationOrder :: (ModC r) => [Module 'SolveModules] -> Sem r [Module 'ResolveImports]
findCompilationOrder ms = do
    depMods <- determineDeps ms
    map coercePass <$> solveModuleOrder (zip depMods ms)

annotateImports :: (ModC r, Member (State (Map (Name 'ResolveImports) ModSig)) r) 
                => Module 'ResolveImports 
                -> Sem r (Module NextPass)
annotateImports = undefined 

