module Language.Cobble.SemAnalysis where

import Language.Cobble.Prelude

import Language.Cobble.Types
import Language.Cobble.Types.Lens

import Data.List ((\\))

type NextPass = Typecheck

data SemanticError = MissingField LexInfo UnqualifiedName
                   | NonExistantOrDuplicateFields LexInfo [UnqualifiedName]
                   | DuplicateStructDefFields LexInfo [UnqualifiedName]
                   deriving (Show, Eq)

runSemanticAnalysis :: Members '[Error SemanticError] r => Module SemAnalysis -> Sem r (Module NextPass)
runSemanticAnalysis (Module x n sts) = fmap (coercePass . Module x n)
    $   transformBiM checkStructDef
    =<< transformBiM checkAndReorderStructConstruct sts


checkStructDef :: Members '[Error SemanticError] r => Statement SemAnalysis -> Sem r (Statement SemAnalysis)
checkStructDef (DefStruct IgnoreExt li name fields) = DefStruct IgnoreExt li name
    <$> detectDuplicateFields li fields
checkStructDef x = pure x

checkAndReorderStructConstruct :: Members '[Error SemanticError] r => Expr SemAnalysis -> Sem r (Expr SemAnalysis)
checkAndReorderStructConstruct (StructConstruct def li n fs) = StructConstruct def li n
    <$> reorderAndCheckFields li (view structFields def) fs
checkAndReorderStructConstruct x = pure x




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
