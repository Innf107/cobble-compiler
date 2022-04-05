module Language.Cobble.SemAnalysis where

import Language.Cobble.Prelude

import Language.Cobble.Types
import Language.Cobble.Types.Lens

import Data.Set qualified as S

type NextPass = Typecheck

data SemanticError = MissingField LexInfo UnqualifiedName
                   | NonExistantOrDuplicateFields LexInfo (Seq UnqualifiedName)
                   | DuplicateStructDefFields LexInfo (Seq UnqualifiedName)
                   | MissingTyClassMethod LexInfo QualifiedName Type
                   | DuplicateTyClassMethods LexInfo (Seq (Decl SemAnalysis))
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

    (DefInstance (defMeths, ps, isImported) li cname ty meths) -> 
        DefInstance (map (\(n, t) -> (n, coercePass $ addForall $ addConstraint isImported t)) defMeths, ps, isImported) li cname ty meths
          where
            addConstraint True t = t
            addConstraint False t = case ps of 
                (p :<| Empty) -> TConstraint (MkConstraint cname (TVar p)) t
                _   -> error $ "SemAnalysis.implicitClassConstraints: multi-param typeclasses NYI: " <> show ps
    
    x -> x

addForall :: Type -> Type
addForall t@TForall{} = t
addForall t = case freeTVsOrdered t of
    []      -> t
    freeTVs -> TForall freeTVs t

detectDuplicateFields :: Members '[Error SemanticError] r => LexInfo -> Seq (UnqualifiedName, a) -> Sem r (Seq (UnqualifiedName, a))
detectDuplicateFields li xs = case ks \\ ordNub ks of
    [] -> pure xs
    leftOver -> throw (DuplicateStructDefFields li leftOver)
    where
        ks = map fst xs

reorderAndCheckFields :: forall a b r. Members '[Error SemanticError] r
                      => LexInfo
                      -> Seq (UnqualifiedName, a)
                      -> Seq (UnqualifiedName, b)
                      -> Sem r (Seq (UnqualifiedName, b))
reorderAndCheckFields li dfs fs = forM dfs \(n, _) -> do
        case lookup n fsMap of
            Nothing -> throw (MissingField li n)
            Just b -> pure (n, b)
    <* case map fst fs \\ map fst dfs of
        [] -> pure ()
        mfs -> throw (NonExistantOrDuplicateFields li mfs)
    where
        fsMap :: Map UnqualifiedName b
        fsMap = fromList $ toList fs


-- Reorders the methods in a typeclass instance to have the same
-- order as the class declaration and checks for missing or duplicate methods.
-- This is necessary for Codegen and expected by the typechecker
reorderAndCheckInstances :: Members '[Error SemanticError] r => Statement SemAnalysis -> Sem r (Statement SemAnalysis)
reorderAndCheckInstances = \case
    DefInstance (defMeths, classPS, isImported) li className ty decls -> do
        reorderedDecls <- forM defMeths \(dmName, dmType) -> do
            case lookup dmName declMap of
                Nothing -> throw $ MissingTyClassMethod li dmName dmType
                Just decl -> pure decl
        case decls `diffEq` reorderedDecls of
            [] -> pure (DefInstance (map (second coercePass) defMeths, coercePass classPS, isImported) li className ty reorderedDecls)
            ds -> throw $ DuplicateTyClassMethods li ds
        where
            declMap :: Map QualifiedName (Decl SemAnalysis)
            declMap = fromList $ toList (map (\d@(Decl _ n _ _) -> (n, d)) decls)
    x -> pure x
