{-# LANGUAGE OverloadedLists #-}
module Cobble.Core.Lower where

import Cobble.Prelude

import Cobble.Types (QualifiedName)
import Cobble.Types qualified as C

import Cobble.Core.Types qualified as F
import Cobble.Util.Bitraversable (secondM)

import Cobble.Codegen.PrimOp

import Cobble.Util.Polysemy.Fresh

import Data.Sequence qualified as S

import GHC.Show qualified as Show
import Data.Text qualified as T

type CExpr = C.Expr C.Codegen
type CStatement = C.Statement C.Codegen
type CModule = C.Module C.Codegen

lower :: (Trace, Members '[Fresh Text QualifiedName] r) => CModule -> Sem r (Seq F.Decl)
lower (C.Module _deps mname sts) = lowerStmnts sts

lowerStmnts :: (Trace, Members '[Fresh Text QualifiedName] r) => (Seq CStatement) -> Sem r (Seq F.Decl)
lowerStmnts Empty = pure []
lowerStmnts (C.Def _ _ (C.Decl _ x [] e) ty :<| sts) = (<|)
    <$> do
        F.Def x
            <$> lowerType ty
            <*> lowerExpr e
    <*> lowerStmnts sts
lowerStmnts (s@C.Def{} :<| sts) = error "lowerStmnts: Definition with arguments persists after typechecking"
lowerStmnts (C.DefClass k li cname tvs methSigs :<| sts) = do
    tvs' <- traverse (\(C.MkTVar x k) -> (x,) <$> lowerKind k) tvs
    k' <- lowerKind k

    dictDef <- F.DefDict cname tvs' <$> traverse (secondM (lowerType . stripConstraint)) methSigs

    methImpls <- forM methSigs \(methName, methTy) -> do
        dictName <- freshVar "d"
        methTy' <- lowerType methTy
        let tvAbs e = foldr (uncurry F.TyAbs) e tvs'
        let dictTy = foldl' (\r (x, k) -> F.TApp r (F.TVar x k)) (F.TCon cname k') tvs'
        pure $ F.Def methName methTy'
            $ tvAbs $ tyAbsFor 1 methTy' $ F.Abs dictName F.TEffUR dictTy $ tyAppsFor 1 methTy' $ F.DictAccess (F.Var dictName) cname (map (uncurry F.TVar) tvs') methName

    ((dictDef :<| methImpls) <>) <$> lowerStmnts sts
        where
            stripConstraint (C.TForall tvs ty) = C.TForall tvs (stripConstraint ty)
            stripConstraint (C.TConstraint _ ty) = ty
            stripConstraint ty = ty

            tyAbsFor (0 :: Int) (F.TForall tv k ty) = F.TyAbs tv k . tyAbsFor 0 ty
            tyAbsFor n (F.TForall _ _ ty) = tyAbsFor (n - 1) ty
            tyAbsFor _ _ = id

            tyAppsFor (0 :: Int) (F.TForall tv k ty) = tyAppsFor 0 ty . flip F.TyApp (F.TVar tv k)
            tyAppsFor n (F.TForall _ _ ty) = tyAppsFor (n - 1) ty
            tyAppsFor _ _ = id

lowerStmnts (C.DefInstance (classKind, _, tyParams, dictVar) li className tyArg methDecls :<| sts) = do
    tyArg' <- lowerType tyArg

    dictKind <- lowerKind classKind

    let dictType = F.TApp (F.TCon className dictKind) tyArg'
    def <- F.Def dictVar dictType . F.DictConstruct className [tyArg'] <$> forM methDecls \(C.Decl _ _ params expr) -> do 
            expr' <- lowerExpr expr
            foldrM (\(x, ty) r -> F.Abs x F.TEffUR <$> lowerType ty <*> pure r) expr' params
    (def :<|) <$> lowerStmnts sts
lowerStmnts (C.DefVariant _ _ x args clauses :<| sts) = do
    def <- F.DefVariant x 
            <$> traverse (\(C.MkTVar x k) -> (x,) <$> lowerKind k) args
            <*> traverse (\(x, tys, _) -> (x,) <$> traverse lowerType tys) clauses
    (def<|) <$> lowerStmnts sts
lowerStmnts (C.DefEffect k li name tvs ops :<| sts) = do
    def <- F.DefEffect name
            <$> traverse (\(C.MkTVar x k) -> (x,) <$> lowerKind k) tvs
            <*> traverse (secondM lowerType) ops
    -- TODO: Also generate fun defs for ops which just `perform` the effect.
    (def<|) <$> lowerStmnts sts

lowerExpr :: (Trace, Members '[Fresh Text QualifiedName] r) => CExpr -> Sem r F.Expr
lowerExpr (C.VariantConstr (_ty, constrTy, ix) _ constrName) = do
    ty <- lowerType constrTy 
    lowerSaturated ty (F.VariantConstr constrName ix)
lowerExpr (C.App ty _ e1 e2) = F.App
                               <$> lowerExpr e1
                               <*> lowerExpr e2
lowerExpr (C.IntLit () _ n) = pure $ F.IntLit n
lowerExpr (C.If ty _ c th el) = F.If 
                                <$> lowerExpr c
                                <*> lowerExpr th
                                <*> lowerExpr el
lowerExpr (C.Let () _ (C.Decl (ty, _) f xs e1) e2) = do
    e1' <- lowerExpr e1
    F.Let f
        <$> lowerType ty
        <*> (foldrM (\(x, t) r -> F.Abs x F.TEffUR <$> lowerType t <*> pure r) e1' xs)
        <*> lowerExpr e2
lowerExpr (C.Var _ _ x) = do
    case lookup x primOps of
        Just (PrimOpInfo primOp primOpType) -> do
            actualTy <- lowerType primOpType
            lowerSaturated actualTy (F.PrimOp primOp actualTy)
        Nothing -> pure $ F.Var x
lowerExpr (C.Case t li e branches) = lowerCase t e branches
lowerExpr (C.Lambda (ty, argTy, eff) _ x e) = F.Abs x <$> lowerType eff <*> lowerType argTy <*> lowerExpr e
lowerExpr (C.TyApp _ ty e) = F.TyApp <$> lowerExpr e <*> lowerType ty
lowerExpr (C.TyAbs _ (C.MkTVar tvName tvKind) e) = F.TyAbs tvName <$> lowerKind tvKind <*> lowerExpr e
lowerExpr (C.DictAbs _ x c e) = F.Abs x F.TEffUR <$> lowerConstraint c <*> lowerExpr e
lowerExpr (C.DictVarApp li e dv) = error $ "Invalid unsubstituted dictionary variable application during lowering at " <> show li <> ".\nApplication of dict variable '" <> show dv <> "'\nOn Expression: " <> show e
lowerExpr (C.DictApp _ e dict) = F.App <$> lowerExpr e <*> pure (F.Var dict)

lowerSaturated :: Members '[Fresh Text QualifiedName] r => F.Type -> (Seq F.Type -> Seq F.Expr -> F.Expr) -> Sem r F.Expr
lowerSaturated (F.TForall a k ty) cont = do
    a' <- freshVar (C.originalName a)
    F.TyAbs a' k <$> lowerSaturated (F.replaceTVar a (F.TVar a' k) ty) (\tyArgs valArgs -> cont (F.TVar a' k <| tyArgs) valArgs)
lowerSaturated (F.TFun t1 eff t2) cont = do
    x <- freshVar "x"
    F.Abs x eff t1 <$> lowerSaturated t2 (\tyArgs valArgs -> cont tyArgs (F.Var x <| valArgs))
lowerSaturated ty cont = pure $ cont [] []

newtype PatternMatrix = MkPatternMatrix (Seq PMatrixRow) deriving (Generic, Data)

type PMatrixRow = (Seq (C.Pattern C.Codegen), Seq F.Expr -> F.Expr)

instance Show.Show PatternMatrix where
    show (MkPatternMatrix seqs) = toString $ unlines $ toList $ map showRow seqs
        where
            showSimplePattern :: C.Pattern C.Codegen -> Text
            showSimplePattern (C.VarP _ x) = show x
            showSimplePattern (C.ConstrP _ con []) = show con
            showSimplePattern (C.ConstrP _ con pats) = show con <> "(" <> intercalate ", " (map showSimplePattern pats) <> ")"
            showSimplePattern (C.IntP _ n) = show n
            showSimplePattern (C.WildcardP _) = "_"
            showSimplePattern (C.OrP _ pats) = "(" <> intercalate " | " (map showSimplePattern pats) <> ")"

            showRow (pats, e) = "| " <> intercalate ", " (map (T.justifyLeft 10 ' ' . showSimplePattern) pats)
                                <> " -> " <> show (e dummyArgs) <> " |"
                where
                    dummyArgs = replicate (length (concatMap boundVars pats)) (F.Var (C.internalQName "#"))

data HeadConstr = ConstrHead QualifiedName Int (Seq C.Type) (Seq QualifiedName) -- TODO: Do we even need types here?
                                --         ^constr index ^ other constructors for the type
                | IntHead Int
                deriving (Show, Eq, Ord, Generic, Data)


isWildcardLike :: C.Pattern C.Codegen -> Bool
isWildcardLike C.VarP{} = True
isWildcardLike C.WildcardP{} = True
isWildcardLike _ = False

isVar :: C.Pattern C.Codegen -> Bool
isVar C.VarP{} = True
isVar _ = False

-- If the first column in the matrix contains exclusively wildcard-like
-- patterns, there is no need to actually match on the first argument, so
-- we can simply remove that column from the matrix.
-- We still have to bind all contained variables though.
findAndBindFirstColFullWildcards :: Seq QualifiedName -> PatternMatrix -> Maybe PatternMatrix
findAndBindFirstColFullWildcards occs (MkPatternMatrix rows) = MkPatternMatrix <$> zipWithM asFirstMaybeWildcard occs rows
    where
        asFirstMaybeWildcard occ (C.VarP{} :<| xs, e) = Just (xs, \es -> e (F.Var occ <| es))
        asFirstMaybeWildcard occ (C.WildcardP{} :<| xs, e) = Just (xs, e)
        asFirstMaybeWildcard _ _ = Nothing

findHeadConstrs :: PatternMatrix -> Seq HeadConstr
findHeadConstrs (MkPatternMatrix rows) = ordNub $ concatMap asFirstConstr rows
    where
        asFirstConstr ((C.IntP _ i :<| _), _) = [IntHead i]
        asFirstConstr ((C.ConstrP (_,i,v) c pats :<| _), _) = case v of
            C.VariantType _ constrs -> [ConstrHead c i (map C.getType pats) (map fst constrs)]
            _ -> error "lowerCase: uncaught pattern match on non-variant constructor (how did this even happen?!)"
        asFirstConstr ((C.OrP _ pats :<| _), e) = concatMap (asFirstConstr . (,e) . pure) pats
        asFirstConstr _ = []

deconstructPM :: QualifiedName -> HeadConstr -> PatternMatrix -> Sem r PatternMatrix
deconstructPM occ pat (MkPatternMatrix rows) = MkPatternMatrix . fold <$> traverse (go pat) rows
    where
        go :: HeadConstr -> PMatrixRow -> Sem r (Seq PMatrixRow)
        go (IntHead i) (C.IntP _ j :<| pats, e)
            | i == j = pure [(pats, e)]
        go (IntHead i) ((C.VarP _ v) :<| pats, e) = pure [(pats, \es -> e (F.IntLit i <| es))]
        go (IntHead i) ((C.WildcardP _) :<| pats, e) = pure [(pats, e)]
        go (ConstrHead c _ _ _) (C.ConstrP _ c' subPats :<| pats, e)
            | c == c' = pure [(subPats <> pats, e)]
        go (ConstrHead _ _ constrArgs _) (C.VarP{} :<| pats, e) = do
            let subPats = map (C.WildcardP) constrArgs
            pure [(subPats <> pats, \es -> e (F.Var occ <| es))]
        go (ConstrHead _ _ constrArgs _) (C.WildcardP _ :<| pats, e) = do
            let subPats = map (C.WildcardP) constrArgs
            pure [(subPats <> pats, e)]
        go h (C.OrP _ (sp1 :<| subPats) :<| pats, e) =
            let sp1Vars = boundVars sp1 in
            (<>)
                <$> go h (sp1 :<| pats, e)
                <*> (fold <$> traverse (\p -> go h (p :<| pats, e . (boundVars p `reorderBy` sp1Vars))) subPats)
        go h (C.OrP _ Empty :<| _, _) = error "lowerCase: deconstructPM: empty or pattern"    
        go _ _ = pure []

defaultMatrix :: QualifiedName -> PatternMatrix -> PatternMatrix
defaultMatrix occ (MkPatternMatrix rows) = MkPatternMatrix $ go =<< rows
    where
        go (C.ConstrP{} :<| _, _) = []
        go (C.IntP{} :<| _, _) = []
        go (C.VarP _ x :<| ps, e) = [(ps, \es -> e (F.Var occ <| es))]
        go (C.WildcardP _ :<| ps, e) = [(ps, e)]
        go (C.OrP _ (sp1 :<| subPats) :<| ps, e) =
            go (sp1 :<| subPats, e)
            <>
            concatMap 
                (\p -> go (p :<| ps, e . (boundVars p `reorderBy` boundVars sp1)))
                subPats
        go (C.OrP _ Empty :<| _, _) = error $ "lowerCase: defaultMatrix: empty or pattern"
        go (Empty, _) = error $ "lowerCase: trying to construct default matrix from a pattern matrix with width 0: " <> show (MkPatternMatrix rows)

boundVars :: C.Pattern C.Codegen -> Seq QualifiedName
boundVars C.IntP{} = []
boundVars (C.VarP _ x) = [x]
boundVars (C.ConstrP _ _ xs) = concatMap boundVars xs
boundVars C.WildcardP{} = []
boundVars (C.OrP _ (p :<| _)) = boundVars p
boundVars (C.OrP _ Empty) = error "lowerCase: boundVars: empty or pattern"

-- | Takes two lists with identical elements and produces
-- a function that takes a list and permutes it, such that
-- ∀xs ys. reorderBy xs ys xs = ys
-- O(n^2)
reorderBy :: Eq a => Seq a -> Seq a -> (forall b. Seq b -> Seq b)
reorderBy xs (y :<| ys) zs = runWithError $ do
    ix <- findIndexL (==y) xs
    S.lookup ix zs <&> (:<| reorderBy xs ys zs)
        where
            runWithError Nothing = error "reorderBy: Invalid combination of lists"
            runWithError (Just r) = r
reorderBy _ Empty _ = Empty

compilePMatrix :: forall r. (Trace, Members '[Fresh Text QualifiedName] r) => Seq QualifiedName -> PatternMatrix -> Sem r F.Expr
compilePMatrix occs m | trace DebugVerbose ("compiling pattern matrix with args " <> show occs <> ":\n" <> show m) False = error "unreachable"
compilePMatrix occs (MkPatternMatrix Empty) = error "Non-exhaustive patterns. (This should not be a panic)"
compilePMatrix occs (MkPatternMatrix ((row, expr) :<| _))
    | all isWildcardLike row = pure $ expr (catMaybes $ zipWith (\occ pat -> if isVar pat then Just (F.Var occ) else Nothing) occs row)
compilePMatrix (occ :<| occs) m
    | Just m' <- findAndBindFirstColFullWildcards (occ :<| occs) m = compilePMatrix occs m'
compilePMatrix (occ :<| occs) m = do
    let headConstrs = findHeadConstrs m
    when (null headConstrs) $ error $ "lowerCase: No head constructors found in supposedly non-empty pattern matrix row. Pattern matrix:\n" <> show m
    fmap (F.Case occ) $ addDefaultIfIncomplete headConstrs =<< traverse compileHeadConstr headConstrs
    where
        compileHeadConstr :: HeadConstr -> Sem r (F.Pattern, F.Expr)
        compileHeadConstr h@(IntHead i) = fmap (F.PInt i,) $ compilePMatrix occs =<< deconstructPM occ h m
        compileHeadConstr h@(ConstrHead c i argTys _) = do
            traceM DebugVerbose $ "compiling pattern matrix with head constructor: " <> show c
            argVars <- traverse (\ty -> (,) <$> freshVar @_ @_ @r "c" <*> lowerType ty) argTys
            fmap (F.PConstr c (argVars) i,) $ compilePMatrix (fmap fst argVars <> occs) =<< deconstructPM occ h m
        
        addDefaultIfIncomplete :: Seq HeadConstr -> Seq (F.Pattern, F.Expr) -> Sem r (Seq (F.Pattern, F.Expr)) 
        addDefaultIfIncomplete headConstrs cases = case headConstrs of -- We can assume that headConstrs is non-empty
            (ConstrHead _ _ _ otherConstrs :<| _) 
                | isComplete headConstrs otherConstrs -> pure cases
            _ -> do
                traceM DebugVerbose "compiling default matrix"
                defaultClause <- (F.PWildcard,) <$> compilePMatrix occs (defaultMatrix occ m)
                pure (cases |> defaultClause)
        
        isComplete headConstrs otherConstrs = S.sort (fmap constrName headConstrs) == S.sort otherConstrs

        constrName (ConstrHead x _ _ _) = x
        constrName c = error $ "lowerCase: mixing variant and non-variant head constructor patterns: " <> show c

compilePMatrix occs m = error $ "lowerCase: compilePMatrix: Invalid occurance/pattern matrix combination:"
                                <> "\n    " <> show occs 
                                <> "\n    " <> show m

collectVarPatTypes :: C.Pattern C.Codegen -> Sem r (Seq (QualifiedName, F.Type))
collectVarPatTypes (C.VarP ty x) = lowerType ty <&> \ty' -> [(x, ty')]
collectVarPatTypes C.IntP{} = pure []
collectVarPatTypes (C.ConstrP _ _ pats) = fold <$> traverse collectVarPatTypes pats
collectVarPatTypes (C.WildcardP _ty) = pure []
-- We always choose the first branch of our or pattern for this.
-- Once we actually bind variables, we can just reorder them to have the same order as the ones in the first branch.
collectVarPatTypes (C.OrP _ (p :<| _)) = collectVarPatTypes p
collectVarPatTypes (C.OrP _ Empty) = error "collectVarPatTypes: empty or pattern"

-- Case desugaring is based on https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf
lowerCase :: forall r. (Trace, Members '[Fresh Text QualifiedName] r)
          => C.Type 
          -> C.Expr C.Codegen 
          -> Seq (C.CaseBranch C.Codegen)
          -> Sem r F.Expr
lowerCase ty expr branches = do
    (jpDefs, possibleBranches) <- unzip <$> forM branches \(C.CaseBranch () _ p e) -> do
        jpName <- freshVar "j"
        e' <- lowerExpr e
        argTypes <- collectVarPatTypes p
        resultTy <- lowerType (C.getType e)
        pure (F.Join jpName [] argTypes e' -- ?
            , ([p], \valArgs -> F.Jump jpName [] valArgs resultTy)) -- ?

    let pmatrix = MkPatternMatrix $ possibleBranches

    expr' <- lowerExpr expr
    exprTy' <- lowerType (C.getType expr)

    scrutName <- freshVar "s"

    caseExpr <- F.Let scrutName exprTy' expr' <$> compilePMatrix [scrutName] pmatrix
    pure $ foldr ($) caseExpr jpDefs -- prepend join point definitions

lowerType :: C.Type -> Sem r F.Type
lowerType (C.TVar (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind
lowerType (C.TCon tcName tcKind) = F.TCon tcName <$> lowerKind tcKind
lowerType (C.TApp t1 t2) = F.TApp <$> lowerType t1 <*> lowerType t2
lowerType (C.TSkol _ (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind
lowerType (C.TForall tvs ty) = do
    ty' <- lowerType ty
    foldrM (\(C.MkTVar tvName tvKind) r -> lowerKind tvKind <&> \k' -> F.TForall tvName k' r) ty' tvs
lowerType (C.TFun t1 eff t2) = F.TFun <$> lowerType t1 <*> lowerType eff <*> lowerType t2
lowerType (C.TConstraint c ty) = F.TFun <$> lowerConstraint c <*> pure F.TEffUR <*> lowerType ty -- Constraints are desugard to dictionary applications
lowerType (C.TRowClosed tys) = do
    tys' <- traverse lowerType tys
    pure (F.TRowExtend tys' F.TRowNil)
lowerType (C.TRowOpen tys (C.MkTVar var kind)) = do
    tys' <- traverse lowerType tys
    kind' <- lowerKind kind
    pure (F.TRowExtend tys' (F.TVar var kind'))
lowerType (C.TRowSkol tys _ (C.MkTVar var kind)) = do
    tys' <- traverse lowerType tys
    kind' <- lowerKind kind
    pure (F.TRowExtend tys' (F.TVar var kind'))

lowerConstraint :: C.Constraint -> Sem r F.Type
lowerConstraint (C.MkConstraint cname k ty) = do
    k' <- lowerKind k
    F.TApp (F.TCon cname k') <$> lowerType ty

lowerKind :: C.Kind -> Sem r F.Kind
lowerKind C.KStar = pure F.KType
lowerKind (C.KFun a b)= F.KFun <$> lowerKind a <*> lowerKind b
lowerKind C.KConstraint = pure F.KType -- Constraints are desugared to dictionary applications
lowerKind C.KEffect = pure F.KEffect
lowerKind (C.KRow k) = F.KRow <$> lowerKind k
