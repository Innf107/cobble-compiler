module Language.Cobble.SemAnalysis where

import Language.Cobble.Prelude

import Language.Cobble.Types
import Language.Cobble.Types.Lens

import Data.List ((\\))

import Data.Set qualified as S

type NextPass = Typecheck

data SemanticError = MissingField LexInfo UnqualifiedName
                   | NonExistantOrDuplicateFields LexInfo [UnqualifiedName]
                   | DuplicateStructDefFields LexInfo [UnqualifiedName]
                   | MissingTyClassMethod LexInfo QualifiedName Type
                   | DuplicateTyClassMethods LexInfo [Decl SemAnalysis]
                   deriving (Show, Eq, Generic, Data)

runSemanticAnalysis :: Members '[Error SemanticError] r => Module SemAnalysis -> Sem r (Module NextPass)
runSemanticAnalysis (Module x n sts) = fmap (coercePass . Module x n)
    $ transformBiM reorderAndCheckInstances 
    (transformBi (implicitForall . implicitClassConstraints) sts)


-- inserts an implicit forall on every type signature. 
-- This function *cannot* just @transformBi@ over @Type@s, since
-- that would also apply on nested types.
implicitForall :: Statement SemAnalysis -> Statement SemAnalysis
implicitForall = \case
    (Def x l d t) -> Def x l d (addForall t)
    x -> x

implicitClassConstraints :: Statement SemAnalysis -> Statement SemAnalysis
implicitClassConstraints = \case
    (DefClass k li cname ps meths) -> 
        DefClass k li cname ps
        $ map (\(n, t) -> (n, addForall $ addConstraint t)) meths
          where
            addConstraint t = case ps of 
                [p] -> TConstraint (MkConstraint cname (TVar p)) t
                _   -> error $ "SemAnalysis.implicitClassConstraints: multi-param typeclasses NYI: " <> show ps

    (DefInstance (defMeths, ps) li cname ty meths) -> 
        DefInstance (map (\(n, t) -> (n, coercePass $ addForall $ addConstraint (coercePass t))) defMeths, ps) li cname ty meths
          where
            addConstraint t = case coercePass ps of 
                [p] -> TConstraint (MkConstraint cname (TVar p)) t
                _   -> error $ "SemAnalysis.implicitClassConstraints: multi-param typeclasses NYI: " <> show ps
    
    x -> x

addForall :: Type -> Type
addForall t@TForall{} = t
-- TODO
-- freeTVs does *not* preserve order. This should not be an issue for now, but might be
-- a bit awkward, if features like type applications, where the order of foralls matters, are ever added.
addForall t = case toList (freeTVs t) of
    []      -> t
    freeTVs -> TForall freeTVs t

detectDuplicateFields :: Members '[Error SemanticError] r => LexInfo -> [(UnqualifiedName, a)] -> Sem r [(UnqualifiedName, a)]
detectDuplicateFields li xs = case ks \\ unstableNub ks of
    [] -> pure xs
    leftOver -> throw (DuplicateStructDefFields li leftOver)
    where
        ks = map fst xs

reorderAndCheckFields :: forall a b r. Members '[Error SemanticError] r
                      => LexInfo
                      -> [(UnqualifiedName, a)]
                      -> [(UnqualifiedName, b)]
                      -> Sem r [(UnqualifiedName, b)]
reorderAndCheckFields li dfs fs = forM dfs \(n, _) -> do
        case lookup n fsMap of
            Nothing -> throw (MissingField li n)
            Just b -> pure (n, b)
    <* case map fst fs \\ map fst dfs of
        [] -> pure ()
        mfs -> throw (NonExistantOrDuplicateFields li mfs)
    where
        fsMap :: Map UnqualifiedName b
        fsMap = fromList fs


-- Reorders the methods in a typeclass instance to have the same
-- order as the class declaration and checks for missing or duplicate methods.
-- This is necessary for Codegen and expected by the typechecker
reorderAndCheckInstances :: Members '[Error SemanticError] r => Statement SemAnalysis -> Sem r (Statement SemAnalysis)
reorderAndCheckInstances = \case
    DefInstance (defMeths, classPS) li className ty decls -> do
        reorderedDecls <- forM defMeths \(dmName, dmType) -> do
            case lookup dmName declMap of
                Nothing -> throw $ MissingTyClassMethod li dmName dmType
                Just decl -> pure decl
        case decls \\ reorderedDecls of
            [] -> pure (DefInstance (map (second coercePass) defMeths, coercePass classPS) li className ty reorderedDecls)
            ds -> throw $ DuplicateTyClassMethods li ds
        where
            declMap :: Map QualifiedName (Decl SemAnalysis)
            declMap = fromList (map (\d@(Decl _ n _ _) -> (n, d)) decls)
    x -> pure x
