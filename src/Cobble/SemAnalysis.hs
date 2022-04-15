module Cobble.SemAnalysis where

import Cobble.Prelude

import Cobble.Types
import Cobble.Types.Lens

import Data.Set qualified as S

type NextPass = Typecheck

data SemanticError = MissingField LexInfo UnqualifiedName
                   | NonExistantOrDuplicateFields LexInfo (Seq UnqualifiedName)
                   | MissingTyClassMethod LexInfo QualifiedName Type
                   | DuplicateTyClassMethods LexInfo (Seq (Decl SemAnalysis))
                   | NonFunctionInEffectOp LexInfo Type
                   deriving (Show, Eq, Generic, Data)

runSemanticAnalysis :: Members '[Error SemanticError] r => Module SemAnalysis -> Sem r (Module NextPass)
runSemanticAnalysis (Module x n sts) = fmap (coercePass . Module x n)
    $ transformBiM reorderAndCheckInstances 
    =<< pure . transformBi (implicitForall . implicitClassConstraints)
    =<< transformBiM implicitEffectType sts


-- inserts an implicit forall on every type signature. 
-- This function *cannot* just @transformBi@ over @Type@s, since
-- that would also apply on nested types.
implicitForall :: Statement SemAnalysis -> Statement SemAnalysis
implicitForall = \case
    (Def x l d t) -> Def x l d (addForall t)
    x -> x

implicitEffectType :: Members '[Error SemanticError] r => Statement SemAnalysis -> Sem r (Statement SemAnalysis)
implicitEffectType = \case
    (DefEffect k li effName args ops) ->
        DefEffect k li effName args <$> traverse (\(opName, opTy) -> (opName,) <$> insertEffect opTy) ops
            where
                insertEffect (TForall tvs ty) = TForall tvs <$> insertEffect ty
                insertEffect (TFun a effs b) = pure $ TFun a (consEff (foldl' TApp (TCon effName KEffect) (map TVar args)) effs) b
                insertEffect ty = throw $ NonFunctionInEffectOp li ty

                consEff eff (TRowClosed effs) = TRowClosed (eff <| effs)
                consEff eff (TRowOpen effs var) = TRowOpen (eff <| effs) var
                consEff eff (TRowSkol effs skol var) = TRowSkol (eff <| effs) skol var
                consEff eff ty = error $ "implicitEffectType: consEff: Non-row type in function effect: " <> show ty
    x -> pure x

implicitClassConstraints :: Statement SemAnalysis -> Statement SemAnalysis
implicitClassConstraints = \case
    (DefClass k li cname ps meths) -> 
        DefClass k li cname ps
        $ map (\(n, t) -> (n, addForall $ addConstraint t)) meths
          where
            addConstraint t = case ps of 
                [p] -> TConstraint (MkConstraint cname k (TVar p)) t
                _   -> error $ "SemAnalysis.implicitClassConstraints: multi-param typeclasses NYI: " <> show ps

    (DefInstance (classKind, defMeths, ps, isImported) li cname ty meths) -> 
        DefInstance (classKind, map (\(n, t) -> (n, coercePass $ addForall $ addConstraint isImported t)) defMeths, ps, isImported) li cname ty meths
          where
            addConstraint True t = t
            addConstraint False t = case ps of 
                (p :<| Empty) -> insertAfterForalls (TConstraint (MkConstraint cname classKind (TVar p))) t
                _   -> error $ "SemAnalysis.implicitClassConstraints: multi-param typeclasses NYI: " <> show ps
    
    x -> x

addForall :: Type -> Type
addForall t@TForall{} = t
addForall t = case freeTVsOrdered t of
    []      -> t
    freeTVs -> TForall freeTVs t

insertAfterForalls :: (Type -> Type) -> Type -> Type
insertAfterForalls f (TForall tvs ty) = TForall tvs (insertAfterForalls f ty)
insertAfterForalls f ty               = f ty

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
    DefInstance (classKind, defMeths, classPS, isImported) li className ty decls -> do
        reorderedDecls <- forM defMeths \(dmName, dmType) -> do
            case lookup dmName declMap of
                Nothing -> throw $ MissingTyClassMethod li dmName dmType
                Just decl -> pure decl
        case decls `diffEq` reorderedDecls of
            [] -> pure (DefInstance (classKind, map (second coercePass) defMeths, coercePass classPS, isImported) li className ty reorderedDecls)
            ds -> throw $ DuplicateTyClassMethods li ds
        where
            declMap :: Map QualifiedName (Decl SemAnalysis)
            declMap = fromList $ toList (map (\d@(Decl _ n _ _) -> (n, d)) decls)
    x -> pure x
