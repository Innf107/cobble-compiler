{-# LANGUAGE OverloadedLists #-}
module Cobble.Core.Lower where

import Cobble.Prelude

import Cobble.Syntax (QualifiedName)
import Cobble.Syntax qualified as C

import Cobble.Core.Syntax qualified as F
import Cobble.Util.Bitraversable (secondM)

import Cobble.Codegen.PrimOp

import Cobble.Util.Polysemy.Fresh

import Data.Sequence qualified as S
import Data.Map qualified as M

import GHC.Show qualified as Show
import Data.Text qualified as T

type CExpr = C.Expr C.Codegen
type CStatement = C.Statement C.Codegen
type CModule = C.Module C.Codegen

data LowerOptions = LowerOptions {
    keepCoreResiduals :: Bool
} deriving (Show, Eq, Generic, Data)

defaultLowerOptions :: LowerOptions
defaultLowerOptions = LowerOptions {
    keepCoreResiduals = False
}

lower :: (Trace, Members '[Fresh Unique, Reader LowerOptions] r) => CModule -> Sem r (Seq F.Decl)
lower (C.Module _deps _mname sts) = lowerStmnts sts

lowerStmnts :: (Trace, Members '[Fresh Unique, Reader LowerOptions] r) => (Seq CStatement) -> Sem r (Seq F.Decl)
lowerStmnts Empty = pure []
lowerStmnts (C.Def _ _ (C.Decl _ x [] e) ty :<| sts) = (<|)
    <$> do
        F.Def x
            <$> lowerType ty
            <*> lowerExpr e
    <*> lowerStmnts sts
lowerStmnts (C.Def{} :<| _sts) = error "lowerStmnts: Definition with arguments persists after typechecking"
lowerStmnts (C.DefClass k _li cname tvs methSigs :<| sts) = do
    tvs' <- traverse (\(C.MkTVar x k) -> (x,) <$> lowerKind k) tvs
    k' <- lowerKind k

    dictDef <- F.DefDict cname tvs' <$> traverse (secondM lowerType) methSigs

    methImpls <- forM methSigs \(methName, methTy) -> do
        dictName <- freshVar "d"
        -- We have to include the constraint and class bound ty vars in
        -- the method implementation, but not in the dictionary definition
        methTy' <- lowerType (C.TForall tv (addConstraint methTy))

        let tvAbs e = foldr (uncurry F.TyAbs) e tvs'
        let dictTy = foldl' (\r (x, k) -> F.TApp r (F.TVar x k)) (F.TCon cname k') tvs'
        pure $ F.Def methName methTy'
            $ tvAbs $ tyAbsFor 1 methTy' $ F.Abs dictName F.TEffUR dictTy $ tyAppsFor 1 methTy' $ F.DictAccess (F.Var dictName) cname (map (uncurry F.TVar) tvs') methName

    ((dictDef :<| methImpls) <>) <$> lowerStmnts sts
        where
            addConstraint (C.TForall tv ty) = C.TForall tv (addConstraint ty)
            addConstraint ty = C.TConstraint (C.MkConstraint cname k (C.TTyVar tv)) ty

            tv = case tvs of
                [tv] -> tv
                _ -> error "Multiparam type classes NYI" 

            tyAbsFor (0 :: Int) (F.TForall tv k ty) = F.TyAbs tv k . tyAbsFor 0 ty
            tyAbsFor n (F.TForall _ _ ty) = tyAbsFor (n - 1) ty
            tyAbsFor _ _ = id

            tyAppsFor (0 :: Int) (F.TForall tv k ty) = tyAppsFor 0 ty . flip F.TyApp (F.TVar tv k)
            tyAppsFor n (F.TForall _ _ ty) = tyAppsFor (n - 1) ty
            tyAppsFor _ _ = id

lowerStmnts (C.DefInstance (classKind, _, _tyParams, dictVar) _li className tyArg methDecls :<| sts) = do
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
lowerStmnts (C.DefEffect _k _li effName tvs ops :<| sts) = do
    tvs' <- traverse (\(C.MkTVar x k) -> (x,) <$> lowerKind k) tvs
    let addForalls ty = foldr (uncurry F.TForall) ty tvs'

    def <- F.DefEffect effName tvs' <$> traverse (secondM lowerType) ops
            
    opDefs <- forM ops \(opName, opTy) -> do
        opTy' <- addForalls <$> lowerType opTy
        let (opTVs, argTy, effTy, _) = decomposeFunTy opTy'
        argName <- freshVar "x"
        pure $ F.Def opName opTy' 
            (foldr (uncurry F.TyAbs) 
                (F.Abs argName effTy argTy (F.Perform effName opName (map (uncurry F.TVar) opTVs) [F.Var argName])) 
                opTVs)
    ((def <| opDefs)<>) <$> lowerStmnts sts

decomposeFunTy :: F.Type -> (Seq (QualifiedName, F.Kind), F.Type, F.Effect, F.Type)
decomposeFunTy (F.TFun dom eff cod) = ([], dom, eff, cod)
decomposeFunTy (F.TForall tv k ty) = let (tvs, dom, eff, cod) = decomposeFunTy ty
                                   in ((tv, k) <| tvs, dom, eff, cod)
decomposeFunTy ty = error $ "decomposeFunTy: Not a function or forall type: " <> show ty

lowerExpr :: (Trace, Members '[Fresh Unique, Reader LowerOptions] r) => CExpr -> Sem r F.Expr
lowerExpr (C.VariantConstr (_ty, constrTy, ix) _ constrName) = do
    ty <- lowerType constrTy 
    lowerSaturated ty (F.VariantConstr constrName ix)
lowerExpr (C.App _ty _ e1 e2) = F.App
                               <$> lowerExpr e1
                               <*> lowerExpr e2
lowerExpr (C.IntLit () _ n) = pure $ F.IntLit n
lowerExpr (C.If _ty _ c th el) = F.If 
                                <$> lowerExpr c
                                <*> lowerExpr th
                                <*> lowerExpr el
lowerExpr (C.Let () _ (C.Decl ty f xs e1) e2) = do
    e1' <- lowerExpr e1
    F.Let f
        <$> lowerType ty
        <*> (foldrM (\(x, t) r -> F.Abs x F.TEffUR <$> lowerType t <*> pure r) e1' xs)
        <*> lowerExpr e2
lowerExpr (C.Var _ _ x) = pure $ F.Var x

lowerExpr (C.Case t _li e branches) = lowerCase t e branches
lowerExpr (C.Lambda (_ty, argTy, eff) _ x e) = F.Abs x <$> lowerType eff <*> lowerType argTy <*> lowerExpr e
lowerExpr (C.Handle (_ty, eff) _li e handlers mretClause) = do
    handlers' <- forM handlers \(C.EffHandler _ _ con params expr) -> do
        (con, map fst params,) <$> lowerExpr expr

    retClause' <- case mretClause of
        Just (name, body) -> (name,) <$> lowerExpr body
        Nothing -> do
            name <- freshVar "r"
            pure (name, F.Var name)

    eff' <- lowerType eff

    e' <- lowerExpr e

    pure (F.Handle e' eff' handlers' retClause')

lowerExpr (C.Resume _ty _li expr) = F.Resume <$> lowerExpr expr
lowerExpr (C.TyApp _ ty e) = do
    traceM TraceLower $ "[lowerExpr (TyApp _ " <> show ty <> " " <> show e <> ")]"
    F.TyApp <$> lowerExpr e <*> lowerType ty
lowerExpr (C.TyAbs _ (C.MkTVar tvName tvKind) e) = F.TyAbs tvName <$> lowerKind tvKind <*> lowerExpr e
lowerExpr (C.DictAbs _ x c e) = F.Abs x F.TEffUR <$> lowerConstraint c <*> lowerExpr e
lowerExpr (C.DictVarApp li e dv) = error $ "Invalid unsubstituted dictionary variable application during lowering at " <> show li <> ".\nApplication of dict variable '" <> show dv <> "'\nOn Expression: " <> show e
lowerExpr (C.DictApp _ e dict) = F.App <$> lowerExpr e <*> pure (F.Var dict)

lowerSaturated :: Members '[Fresh Unique] r => F.Type -> (Seq F.Type -> Seq F.Expr -> F.Expr) -> Sem r F.Expr
-- We simply don't apply effect variables for saturated types, since those should only have effect ✶ anyway.
-- I hope this is sound...
lowerSaturated (F.TForall a k@(F.KRow F.KEffect) ty) cont = do
    a' <- freshVar (C.originalName a)
    F.TyAbs a' k <$> lowerSaturated (F.replaceTVar a (F.TVar a' k) ty) cont
lowerSaturated (F.TForall a k ty) cont = do
    a' <- freshVar (C.originalName a)
    F.TyAbs a' k <$> lowerSaturated (F.replaceTVar a (F.TVar a' k) ty) (\tyArgs valArgs -> cont (F.TVar a' k <| tyArgs) valArgs)
lowerSaturated (F.TFun t1 eff t2) cont = do
    x <- freshVar "x"
    F.Abs x eff t1 <$> lowerSaturated t2 (\tyArgs valArgs -> cont tyArgs (F.Var x <| valArgs))
lowerSaturated _ty cont = pure $ cont [] []


            

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
        asFirstMaybeWildcard _occ (C.WildcardP{} :<| xs, e) = Just (xs, e)
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
        go (IntHead i) ((C.VarP _ _v) :<| pats, e) = pure [(pats, \es -> e (F.IntLit i <| es))]
        go (IntHead _i) ((C.WildcardP _) :<| pats, e) = pure [(pats, e)]
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
        go _h (C.OrP _ Empty :<| _, _) = error "lowerCase: deconstructPM: empty or pattern"    
        go _ _ = pure []

defaultMatrix :: QualifiedName -> PatternMatrix -> PatternMatrix
defaultMatrix occ (MkPatternMatrix rows) = MkPatternMatrix $ go =<< rows
    where
        go (C.ConstrP{} :<| _, _) = []
        go (C.IntP{} :<| _, _) = []
        go (C.VarP _ _x :<| ps, e) = [(ps, \es -> e (F.Var occ <| es))]
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

compilePMatrix :: forall r. (Trace, Members '[Fresh Unique, Reader LowerOptions] r) => Seq QualifiedName -> PatternMatrix -> Sem r F.Expr
compilePMatrix occs m | trace TraceLower ("compiling pattern matrix with args " <> show occs <> ":\n" <> show m) False = error "unreachable"
compilePMatrix _occs (MkPatternMatrix Empty) = error "Non-exhaustive patterns. (This should not be a panic)"
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
            traceM TraceLower $ "compiling pattern matrix with head constructor: " <> show c
            argVars <- traverse (\ty -> (,) <$> freshVar @r "c" <*> lowerType ty) argTys
            fmap (F.PConstr c (argVars) i,) $ compilePMatrix (fmap fst argVars <> occs) =<< deconstructPM occ h m
        
        addDefaultIfIncomplete :: Seq HeadConstr -> Seq (F.Pattern, F.Expr) -> Sem r (Seq (F.Pattern, F.Expr)) 
        addDefaultIfIncomplete headConstrs cases = case headConstrs of -- We can assume that headConstrs is non-empty
            (ConstrHead _ _ _ otherConstrs :<| _) 
                | isComplete headConstrs otherConstrs -> pure cases
            _ -> do
                traceM TraceLower "compiling default matrix"
                defaultClause <- (F.PWildcard,) <$> compilePMatrix occs (defaultMatrix occ m)
                pure (cases |> defaultClause)
        
        isComplete headConstrs otherConstrs = S.sort (fmap constrName headConstrs) == S.sort otherConstrs

        constrName (ConstrHead x _ _ _) = x
        constrName c = error $ "lowerCase: mixing variant and non-variant head constructor patterns: " <> show c

compilePMatrix occs m = error $ "lowerCase: compilePMatrix: Invalid occurance/pattern matrix combination:"
                                <> "\n    " <> show occs 
                                <> "\n    " <> show m

collectVarPatTypes :: Members '[Reader LowerOptions] r => C.Pattern C.Codegen -> Sem r (Seq (QualifiedName, F.Type))
collectVarPatTypes (C.VarP ty x) = lowerType ty <&> \ty' -> [(x, ty')]
collectVarPatTypes C.IntP{} = pure []
collectVarPatTypes (C.ConstrP _ _ pats) = fold <$> traverse collectVarPatTypes pats
collectVarPatTypes (C.WildcardP _ty) = pure []
-- We always choose the first branch of our or pattern for this.
-- Once we actually bind variables, we can just reorder them to have the same order as the ones in the first branch.
collectVarPatTypes (C.OrP _ (p :<| _)) = collectVarPatTypes p
collectVarPatTypes (C.OrP _ Empty) = error "collectVarPatTypes: empty or pattern"

-- Case desugaring is based on https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf
lowerCase :: forall r. (Trace, Members '[Fresh Unique, Reader LowerOptions] r)
          => C.Type 
          -> C.Expr C.Codegen 
          -> Seq (C.CaseBranch C.Codegen)
          -> Sem r F.Expr
lowerCase _ty expr branches = do
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

lowerType :: (HasCallStack, Members '[Reader LowerOptions] r) => C.Type -> Sem r F.Type
lowerType (C.TTyVar (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind
lowerType ty@(C.TUnif (C.MkTVar tvName tvKind)) = asks keepCoreResiduals >>= \case
    True -> F.TVar tvName <$> lowerKind tvKind
    False -> error $ "lowerType: Residual unification variable: " <> show ty
lowerType (C.TCon tcName tcKind) = F.TCon tcName <$> lowerKind tcKind
lowerType (C.TApp t1 t2) = F.TApp <$> lowerType t1 <*> lowerType t2
lowerType (C.TSkol _ (C.MkTVar tvName tvKind)) = F.TVar tvName <$> lowerKind tvKind
lowerType (C.TForall (C.MkTVar tvName tvKind) ty) = F.TForall tvName <$> lowerKind tvKind <*> lowerType ty
lowerType (C.TFun t1 eff t2) = F.TFun <$> lowerType t1 <*> lowerType eff <*> lowerType t2
lowerType (C.TConstraint c ty) = F.TFun <$> lowerConstraint c <*> pure F.TEffUR <*> lowerType ty -- Constraints are desugard to dictionary applications
lowerType (C.TRowClosed tys) = do
    tys' <- traverse lowerType tys
    pure (F.TRowExtend tys' F.TRowNil)
lowerType (C.TRowVar tys (C.MkTVar var kind)) = do
    tys' <- traverse lowerType tys
    kind' <- lowerKind kind
    pure (F.TRowExtend tys' (F.TVar var kind'))
lowerType ty@C.TRowUnif{} = error $ "lowerType: Residual unification variable in row type: " <> show ty
lowerType (C.TRowSkol tys _ (C.MkTVar var kind)) = do
    tys' <- traverse lowerType tys
    kind' <- lowerKind kind
    pure (F.TRowExtend tys' (F.TVar var kind'))

lowerConstraint :: Members '[Reader LowerOptions] r => C.Constraint -> Sem r F.Type
lowerConstraint (C.MkConstraint cname k ty) = do
    k' <- lowerKind k
    F.TApp (F.TCon cname k') <$> lowerType ty

lowerKind :: C.Kind -> Sem r F.Kind
lowerKind C.KStar = pure F.KType
lowerKind (C.KFun a b)= F.KFun <$> lowerKind a <*> lowerKind b
lowerKind C.KConstraint = pure F.KType -- Constraints are desugared to dictionary applications
lowerKind C.KEffect = pure F.KEffect
lowerKind (C.KRow k) = F.KRow <$> lowerKind k

freshVar :: Members '[Fresh Unique] r => Text -> Sem r QualifiedName
freshVar x = do
    u <- fresh
    pure (C.UnsafeQualifiedName x (C.GeneratedQName u))
