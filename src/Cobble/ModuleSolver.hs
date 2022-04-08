module Cobble.ModuleSolver where

import Cobble.Prelude

import Cobble.Types

type ModC (r :: EffectRow) = Members '[Error ModuleError] r

data ModuleError = CircularDependency (Seq (Seq (Name 'SolveModules)))
                 | ModuleDoesNotExist (Name 'SolveModules)
                 deriving (Show, Eq)

data DepModule = DepModule {
        modName :: Name 'SolveModules
    ,   modDeps :: (Seq DepModule)
    ,   remainingModDeps :: (Seq DepModule)
    } deriving (Show, Eq)

type NextPass = 'QualifyNames

-- | Finds an order for a list of modules such that
-- whenever a module is compiled, all its dependencies have already been compiled
-- (a topological sort).
--
-- This function is not very efficient, but that should
-- not matter even if there are a lot of modules in a project.
-- In case it does matter, the function should be fairly easy to optimize.
solveModuleOrder :: (ModC r) => Seq (DepModule, a) -> Sem r (Seq (DepModule, a))
solveModuleOrder [] = pure []
solveModuleOrder ms = case find (null . remainingModDeps . fst) ms of
    Nothing -> findCircularDeps (map fst ms)
    Just (m, x) -> ((m, x):<|) <$> solveModuleOrder
        (map (first (\dm -> dm{remainingModDeps = removeMod (remainingModDeps dm)})) $ filter ((/=modName m) . modName . fst) ms)
        where
            removeMod :: Seq DepModule -> Seq DepModule
            removeMod = filter ((/=modName m) . modName)
 
findCircularDeps :: (ModC r) => Seq DepModule -> Sem r a
findCircularDeps ms = throw $ CircularDependency $ catMaybes $ map (flip getCycle []) ms
    where
        getCycle :: DepModule -> Seq DepModule -> Maybe (Seq (Name 'SolveModules))
        getCycle (DepModule _  _ []) _ = Nothing
        getCycle m@(DepModule n _ remainingDeps) soFar
            | any ((==n) . modName) soFar = Just (dropWhileL (/=n) $ map modName (m<|soFar))
            | otherwise = concat <$> traverse (flip getCycle (m<|soFar)) remainingDeps


-- TODO!: Detect cycles here
toDepMod :: forall r. (ModC r) => Seq (Module 'SolveModules) -> Module 'SolveModules -> Sem r DepModule
toDepMod ms m@(Module () mname _) = getModDeps m <&> \curDeps -> DepModule mname curDeps curDeps
    where
        getModDeps :: Module 'SolveModules -> Sem r (Seq DepModule)
        getModDeps (Module () _ msts) = msts & wither \case
            Import () _ importedMName -> case find ((==importedMName) . moduleName) ms of
                Nothing -> throw $ ModuleDoesNotExist importedMName
                Just importedMod -> getModDeps importedMod <&> \impDeps -> Just $ DepModule importedMName impDeps impDeps
            _ -> pure Nothing

determineDeps :: (ModC r) => Seq (Module 'SolveModules) -> Sem r (Seq DepModule)
determineDeps ms = traverse (toDepMod ms) ms

flattenDeps :: DepModule -> Seq (Name 'SolveModules)
flattenDeps (DepModule{modDeps}) = map modName modDeps <> concatMap flattenDeps modDeps

findCompilationOrder :: (ModC r) => Seq (Module 'SolveModules) -> Sem r (Seq (Module 'SolveModules, Seq (Name 'SolveModules)))
findCompilationOrder ms = do
    depMods <- determineDeps ms
    map (second flattenDeps . swap) <$> solveModuleOrder (zip depMods ms)

