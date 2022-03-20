{-# LANGUAGE OverloadedLists #-}
module Language.Cobble.Core.Lower where

import Language.Cobble.Prelude

import Language.Cobble.Types (QualifiedName)
import Language.Cobble.Types qualified as C

import Language.Cobble.Core.Types qualified as F
import Language.Cobble.Util.Bitraversable (secondM)

import Language.Cobble.Util.Polysemy.Fresh

import Data.Sequence qualified as S

type CExpr = C.Expr C.Codegen
type CStatement = C.Statement C.Codegen
type CModule = C.Module C.Codegen

lower :: Members '[Fresh Text QualifiedName] r => CModule -> Sem r [F.Decl]
lower (C.Module _deps mname sts) = lowerStmnts sts

lowerStmnts :: Members '[Fresh Text QualifiedName] r => [CStatement] -> Sem r [F.Decl]
lowerStmnts [] = pure []
lowerStmnts (C.Def _ _ (C.Decl _ x [] e) ty : sts) = (:)
    <$> do
        F.Def x
            <$> lowerType ty
            <*> lowerExpr e
    <*> lowerStmnts sts
lowerStmnts (s@C.Def{} : sts) = error "lowerStmnts: Definition with arguments persists after typechecking"
lowerStmnts (C.Import{} : sts) = lowerStmnts sts
lowerStmnts (C.DefClass{} : sts) = undefined
lowerStmnts (C.DefInstance{} : sts) = undefined
lowerStmnts (C.DefVariant _ _ x args clauses : sts) = do
    def <- F.DefVariant x 
            <$> traverse (\(C.MkTVar x k) -> (x,) <$> lowerKind k) args
            <*> traverse (\(x, tys, _) -> (x,) <$> traverse lowerType tys) clauses
    (def:) <$> lowerStmnts sts

lowerExpr :: Members '[Fresh Text QualifiedName] r => CExpr -> Sem r F.Expr
lowerExpr (C.VariantConstr (_ty, constrTy, ix) _ x) = lowerType constrTy >>= \ty -> lowerVariantConstr ty ix x [] []
lowerExpr (C.App ty _ e1 e2) = F.App
                               <$> lowerExpr e1
                               <*> lowerExpr e2
lowerExpr (C.IntLit () _ n) = pure $ F.IntLit n
lowerExpr (C.UnitLit li) = pure $ F.UnitLit
lowerExpr (C.If ty _ c th el) = F.If 
                                <$> lowerExpr c
                                <*> lowerExpr th
                                <*> lowerExpr el
lowerExpr (C.Let () _ (C.Decl (ty, _) f xs e1) e2) = do
    e1' <- lowerExpr e1
    F.Let f
        <$> lowerType ty
        <*> (foldrM (\(x, t) r -> F.Abs x <$> lowerType t <*> pure r) e1' xs)
        <*> lowerExpr e2
lowerExpr (C.Var _ _ x) = pure $ F.Var x
lowerExpr (C.Case t li e branches) = lowerCase t e branches
lowerExpr (C.Lambda (ty, argTy) _ x e) = F.Abs x <$> lowerType argTy <*> lowerExpr e
lowerExpr (C.TyApp _ ty e) = F.TyApp <$> lowerExpr e <*> lowerType ty
lowerExpr (C.TyAbs _ (C.MkTVar tvName tvKind) e) = F.TyAbs tvName <$> lowerKind tvKind <*> lowerExpr e

lowerVariantConstr :: Members '[Fresh Text QualifiedName] r => F.Type -> Int -> QualifiedName -> Seq F.Type -> Seq F.Expr -> Sem r F.Expr
lowerVariantConstr (F.TForall a k ty) i constrName tyArgs valArgs = do
    a' <- freshVar (C.originalName a)
    F.TyAbs a' k <$> lowerVariantConstr (F.replaceTVar a (F.TVar a' k) ty) i constrName (tyArgs |> F.TVar a' k) valArgs
lowerVariantConstr (F.TFun t1 t2) i constrName tyArgs valArgs = do
    x <- freshVar "x"
    F.Abs x t1 <$> lowerVariantConstr t2 i constrName tyArgs (valArgs |> F.Var x)
lowerVariantConstr ty i constrName tyArgs valArgs = do
    pure $ F.VariantConstr constrName i tyArgs valArgs

newtype PatternMatrix = MkPatternMatrix (Seq (Seq (C.Pattern C.Codegen), F.Expr)) deriving (Show, Eq, Generic, Data)

data HeadConstr = ConstrHead QualifiedName [C.Type] -- TODO: Do we even need types here?
                | IntHead Int
                deriving (Show, Eq, Ord, Generic, Data)

width :: PatternMatrix -> Int
width (MkPatternMatrix Empty) = error "Empty matrix has no width"
width (MkPatternMatrix ((row, _) :<| _)) = S.length row

height :: PatternMatrix -> Int
height (MkPatternMatrix m) = S.length m

patternAt :: Int -> Int -> PatternMatrix -> C.Pattern C.Codegen
patternAt i j (MkPatternMatrix rows) = S.index (fst (S.index rows i)) j

isWildcardLike :: C.Pattern C.Codegen -> Bool
isWildcardLike (C.VarP _ _) = True
isWildcardLike _ = False

bindVarPat :: C.Pattern C.Codegen -> F.Expr -> F.Expr -> Sem r F.Expr
bindVarPat (C.VarP ty x) expr body = F.Let x <$> lowerType ty <*> pure expr <*> pure body
bindVarPat _ _ body = pure body

-- If the first column in the matrix contains exclusively wildcard-like
-- patterns, there is no need to actually match on the first argument, so
-- we can simply remove that column from the matrix.
-- (We should still bind the expression somehow though)
findFirstColFullWildcards :: PatternMatrix -> Maybe PatternMatrix
findFirstColFullWildcards (MkPatternMatrix rows) = MkPatternMatrix <$> traverse asFirstMaybeWildcard rows
    where
        asFirstMaybeWildcard (x :<| xs, e)
            | isWildcardLike x = Just (xs, e)
        asFirstMaybeWildcard _ = Nothing

findFirstColNonWildcardConstrs :: PatternMatrix -> Seq HeadConstr
findFirstColNonWildcardConstrs (MkPatternMatrix rows) = ordNub $ mapMaybe asFirstConstr rows
    where
        asFirstConstr ([C.IntP _ i], _)         = Just (IntHead i)
        asFirstConstr ([C.ConstrP _ c pats], _) = Just (ConstrHead c (map C.getType pats))
        asFirstConstr _                         = Nothing

deconstructPM :: HeadConstr -> PatternMatrix -> PatternMatrix
deconstructPM pat (MkPatternMatrix rows) = MkPatternMatrix $ (go pat) =<< rows
    where
        go (IntHead i) (C.IntP _ j :<| pats, e)
            | i == j = [(pats, e)]
        go (ConstrHead c _) (C.ConstrP _ c' subPats :<| pats, e)
            | c == c' = [(fromList subPats <> pats, e)]
        go _ _ = []

compilePMatrix :: forall r. Members '[Fresh Text QualifiedName] r => Seq F.Expr -> PatternMatrix -> Sem r F.Expr
compilePMatrix occs (MkPatternMatrix Empty) = error "Non-exhaustive patterns. (This should not be a panic)"
compilePMatrix occs (MkPatternMatrix ((row, expr) :<| _))
    | all isWildcardLike row = foldrM ($) expr (S.zipWith bindVarPat row occs)
compilePMatrix (_occ :<| occs) m
    | Just m' <- findFirstColFullWildcards m = compilePMatrix occs m' -- TODO: bind variables
compilePMatrix (occ :<| occs) m = do
    let headConstrs = findFirstColNonWildcardConstrs m
    F.Case occ . addDefaultIfIncomplete headConstrs <$> traverse compileHeadConstr headConstrs
    where
        compileHeadConstr:: HeadConstr -> Sem r (F.Pattern, F.Expr)
        compileHeadConstr h@(IntHead i) = (F.PInt i,) <$> compilePMatrix occs (deconstructPM h m)
        compileHeadConstr h@(ConstrHead c argTys) = do
            argVars <- traverse (\ty -> (,) <$> freshVar @_ @_ @r "c" <*> lowerType ty) argTys
            (F.PConstr c (map fst argVars),) <$> compilePMatrix (fromList $ fmap (\(x, _) -> F.Var x) argVars) (deconstructPM h m)
        
        addDefaultIfIncomplete headConstrs cases = undefined
        
compilePMatrix occs m = error $ "lowerCase: compilePMatrix: Invalid occurance/pattern matrix combination:"
                                <> "\n    " <> show occs 
                                <> "\n    " <> show m

-- Case desugaring is based on https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf
lowerCase :: forall r. Members '[Fresh Text QualifiedName] r 
          => C.Type 
          -> C.Expr C.Codegen 
          -> [C.CaseBranch C.Codegen] 
          -> Sem r F.Expr
lowerCase ty expr branches = do
    (jpDefs :: [F.Expr -> F.Expr], possibleBranches :: [(Seq (C.Pattern C.Codegen), F.Expr)]) <- unzip <$> forM branches \(C.CaseBranch () _ p e) -> do
        jpName <- freshVar "j"
        e' <- lowerExpr e
        pure (F.Join jpName undefined undefined e'
            , ([p], F.Jump jpName undefined undefined undefined))

    let pmatrix = MkPatternMatrix $ fromList $ possibleBranches

    expr' <- lowerExpr expr

    caseExpr <- compilePMatrix [expr'] pmatrix
    pure $ foldr ($) caseExpr jpDefs -- prepend join point definitions

lowerType :: C.Type -> Sem r F.Type
lowerType (C.TVar (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind
lowerType (C.TCon tcName tcKind) = F.TCon tcName <$> lowerKind tcKind
lowerType (C.TApp t1 t2) = F.TApp <$> lowerType t1 <*> lowerType t2
lowerType (C.TSkol _ (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind  -- error $ "lowerType: detected escaped skolem type constant: " <> show t
lowerType (C.TForall tvs ty) = do
    ty' <- lowerType ty
    foldrM (\(C.MkTVar tvName tvKind) r -> lowerKind tvKind <&> \k' -> F.TForall tvName k' r) ty' tvs
lowerType (C.TFun t1 t2) = F.TFun <$> lowerType t1 <*> lowerType t2
lowerType (C.TConstraint c ty) = F.TFun <$> lowerConstraint c <*> lowerType ty -- Constraints are desugard to dictionary applications

lowerConstraint :: C.Constraint -> Sem r F.Type
lowerConstraint (C.MkConstraint cname ty) = F.TApp (F.TCon cname (F.KFun F.KType F.KType)) <$> lowerType ty

lowerKind :: C.Kind -> Sem r F.Kind
lowerKind C.KStar = pure F.KType
lowerKind (C.KFun a b)= F.KFun <$> lowerKind a <*> lowerKind b
lowerKind C.KConstraint = pure F.KType -- Constraints are desugared to dictionary applications
