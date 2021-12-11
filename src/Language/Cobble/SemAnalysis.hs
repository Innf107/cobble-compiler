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
                   | MissingTyClassMethod LexInfo QualifiedName (Type Codegen)
                   | DuplicateTyClassMethods LexInfo [Decl SemAnalysis]
                   deriving (Show, Eq)

runSemanticAnalysis :: Members '[Error SemanticError] r => Module SemAnalysis -> Sem r (Module NextPass)
runSemanticAnalysis (Module x n sts) = fmap (coercePass . Module x n)
    $ transformBiM checkStructDef
    =<< transformBiM checkAndReorderStructConstruct 
    =<< transformBiM reorderAndCheckInstances 
    (transformBi (skolemizeAscriptions)
    $ transformBi (implicitForall . implicitClassConstraints) sts)


checkStructDef :: Members '[Error SemanticError] r => Statement SemAnalysis -> Sem r (Statement SemAnalysis)
checkStructDef (DefStruct (Ext k) li name ps fields) = DefStruct (Ext k) li name ps
    <$> detectDuplicateFields li fields
checkStructDef x = pure x

checkAndReorderStructConstruct :: Members '[Error SemanticError] r => Expr SemAnalysis -> Sem r (Expr SemAnalysis)
checkAndReorderStructConstruct (StructConstruct def li n fs) = StructConstruct def li n
    <$> reorderAndCheckFields li (view structFields def) fs
checkAndReorderStructConstruct x = pure x

-- inserts an implicit forall on every type signature. 
-- This function *cannot* just @transformBi@ over @Type@s, since
-- that would also apply on nested types.
implicitForall :: Statement SemAnalysis -> Statement SemAnalysis
implicitForall = \case
    (Def x l d t) -> Def x l d (addForall t)
    x -> x

implicitClassConstraints :: Statement SemAnalysis -> Statement SemAnalysis
implicitClassConstraints = \case
    (DefClass (Ext k) li cname ps meths) -> 
        DefClass (Ext k) li cname ps
        $ map (\(n, t) -> (n, addForall $ addConstraint t)) meths
          where
            addConstraint t = case ps of 
                [p] -> TConstraint (MkConstraint cname (TVar p)) t
                _   -> error $ "SemAnalysis.implicitClassConstraints: multi-param typeclasses NYI: " <> show ps

    (DefInstance (Ext (defMeths, ps)) li cname ty meths) -> 
        DefInstance (Ext (map (\(n, t) -> (n, coercePass $ addForall $ addConstraint (coercePass t))) defMeths, ps)) li cname ty meths
          where
            addConstraint t = case coercePass ps of 
                [p] -> TConstraint (MkConstraint cname (TVar p)) t
                _   -> error $ "SemAnalysis.implicitClassConstraints: multi-param typeclasses NYI: " <> show ps
    
    x -> x

addForall :: Type SemAnalysis -> Type SemAnalysis
addForall t = case ordNub [tv | TVar tv <- universeBi t] of
    []      -> t
    freeTVs -> TForall freeTVs t

skolemizeAscriptions :: Expr SemAnalysis -> Expr SemAnalysis
skolemizeAscriptions (Ascription x l e ascrTy) = Ascription x l e (skolemizeFreeTyvars mempty ascrTy)
    where
        skolemizeFreeTyvars :: Set (TVar SemAnalysis) -> Type SemAnalysis -> Type SemAnalysis
        skolemizeFreeTyvars bound (TVar tv) 
            | tv `member` bound = TVar tv
            | otherwise = TSkol tv
        skolemizeFreeTyvars bound (TForall ps ty) = skolemizeFreeTyvars (foldr S.insert bound ps) ty
        skolemizeFreeTyvars _ t@TCon{} = t
        skolemizeFreeTyvars _ t@TSkol{} = t
        skolemizeFreeTyvars bound (TApp t1 t2) = TApp (skolemizeFreeTyvars bound t1) (skolemizeFreeTyvars bound t2)
        skolemizeFreeTyvars bound (TConstraint (MkConstraint cname ct) ty) = 
            TConstraint (MkConstraint cname (skolemizeFreeTyvars bound ct)) (skolemizeFreeTyvars bound ty)
skolemizeAscriptions e = e


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
    DefInstance (Ext (defMeths, classPS)) li className ty decls -> do
        reorderedDecls <- forM defMeths \(dmName, dmType) -> do
            case lookup dmName declMap of
                Nothing -> throw $ MissingTyClassMethod li dmName dmType
                Just decl -> pure decl
        case decls \\ reorderedDecls of
            [] -> pure (DefInstance (Ext (map (second coercePass) defMeths, coercePass classPS)) li className ty reorderedDecls)
            ds -> throw $ DuplicateTyClassMethods li ds
        where
            declMap :: Map QualifiedName (Decl SemAnalysis)
            declMap = fromList (map (\d@(Decl _ n _ _) -> (n, d)) decls)
    x -> pure x
