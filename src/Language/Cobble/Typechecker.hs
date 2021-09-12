{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Typechecker where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Util.Convert
import Language.Cobble.Util.Bitraversable
import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Util.Polysemy.Dump
import Language.Cobble.Types hiding (Type)
import Language.Cobble.Types qualified as C 
import Language.Cobble.Types.Lens

import qualified Data.Text as T

import qualified Data.Map as M

type NextPass = 'PostProcess

type Type = C.Type NextPass

newtype TCState = TCState {
    _varTypes :: M.Map QualifiedName Type
} deriving (Show, Eq, Generic, Data, Semigroup, Monoid)
makeLenses ''TCState

data TypeError = DifferentConstructor LexInfo Type Type 
               | NotEnoughArgs LexInfo Type Type
               | Occurs LexInfo (TVar NextPass) Type
               | HigherPoly LexInfo Type Type
               | NoStructsForType LexInfo Type
               | AmbiguousStructAccess LexInfo Type [Type]
               deriving (Show, Eq, Generic, Data)

type TConstraint = (TConstraintComp, LexInfo)

data TConstraintComp = Type :~ Type 
                     | OneOf Type [Type]
                     deriving (Show, Eq, Generic, Data)

newtype Substitution = Subst {unSubst :: Map (TVar NextPass) Type} 
    deriving stock   (Show, Eq, Generic, Data)

-- Not sure if this is really associative...
instance Semigroup Substitution where
    Subst s1 <> Subst s2 = Subst $ M.filterWithKey notIdentical $ fmap (applySubst (Subst s2)) s1 <> s2
        where
            notIdentical tv (TVar tv') | tv == tv' = False
            notIdentical _ _ = True

instance Monoid Substitution where
    mempty = Subst mempty

lookupType :: Members '[State TCState, Fresh (TVar NextPass) (TVar NextPass)] r => QualifiedName -> Sem r Type
lookupType v = gets (lookup v . view varTypes) <&> fromMaybe (error $ "lookupType: Typechecker cannot find variable: " <> show v)
    
removeInitialForall :: Members '[Fresh (TVar NextPass) (TVar NextPass)] r => Type -> Sem r Type
removeInitialForall (TForall ps t) = foldrM (\p r -> freshVar p <&> \p' -> replaceTVar p p' r) t ps
removeInitialForall x              = pure x

insertType :: Members '[State TCState] r => QualifiedName -> Type -> Sem r ()
insertType v t = modify (varTypes %~ insert v t)

getType' :: (Members '[Fresh (TVar NextPass) (TVar NextPass)] r, HasType a NextPass) => a -> Sem r Type
getType' = removeInitialForall . getType

checkStmnt :: Members '[Writer [TConstraint], Fresh Text QualifiedName, Fresh (TVar NextPass) (TVar NextPass), State TCState] r 
           => Statement Typecheck 
           -> Sem r (Statement NextPass)
checkStmnt (Import IgnoreExt li mod) = pure $ Import IgnoreExt li mod
checkStmnt (DefStruct (Ext k) li sname ps fields) = pure $ DefStruct (Ext k) li sname (map coercePass ps) (map (second coercePass) fields) 
checkStmnt (Def IgnoreExt li (Decl IgnoreExt f (Ext ps) e) (coercePass -> ty)) = do
    insertType f ty
    ty' <- removeInitialForall ty
    psTys <- traverse (\_ -> freshTV KStar) ps
    zipWithM_ insertType ps psTys

    e' <- check e
    eTy <- getType' e'
    tellLI li [ty' :~ foldr (:->) eTy psTys]
    pure (Def IgnoreExt li (Decl (Ext ty') f (Ext (zip ps psTys)) e') ty)

checkStmnt (StatementX x _) = absurd x

check :: Members '[Writer [TConstraint], Fresh Text QualifiedName, Fresh (TVar NextPass) (TVar NextPass), State TCState] r 
      => Expr Typecheck 
      -> Sem r (Expr NextPass)
check (IntLit IgnoreExt li n)   = pure (IntLit IgnoreExt li n)
check (UnitLit li)              = pure (UnitLit li)
check (Var IgnoreExt li vname)  = Var . Ext <$> lookupType vname <*> pure li <*> pure vname
check (FCall IgnoreExt li f as) = do
    f' <- check f
    as' <- traverse check as
    ret <- freshTV KStar

    fTy <- getType' f'

    asTys <- traverse getType' as'

    tellLI li [fTy :~ (foldr (:->) ret asTys)]
    pure (FCall (Ext ret) li f' as')
check (If IgnoreExt li cond th el) = do
    cond' <- check cond
    condTy <- getType' cond'
    
    th' <- check th
    thTy <- getType' th'
    
    el' <- check el
    elTy <- getType' el'

    tellLI li [condTy :~ boolT, thTy :~ elTy]
    pure (If IgnoreExt li cond' th' el')
check (Let IgnoreExt li (Decl IgnoreExt f (Ext ps) e) body) = do
    fTy <- freshTV KStar
    psTys <- traverse (\_ -> freshTV KStar) ps

    insertType f fTy

    zipWithM_ insertType ps psTys
    e' <- check e
    eTy <- getType' e'
    
    body' <- check body

    tellLI li [fTy :~ foldr (:->) eTy psTys]
    pure (Let IgnoreExt li (Decl (Ext fTy) f (Ext (zip ps psTys)) e') body')
check (StructConstruct structDef li structName fields) = do
    let sTy = coercePass $ structDef ^. structType
    
    psTys <- traverse (freshVar . coercePass) (structDef ^. structParams)
    let polyTypes :: M.Map (TVar NextPass) (TVar NextPass) = fromList $ zipWith (\x y -> (x, coercePass y)) psTys (structDef ^. structParams) 
    -- Application is left associative
    let retTy = foldl' (\r tv -> TApp r (TVar tv)) sTy psTys

    fields' <- zipForM (structDef ^. structFields) fields \(name, (coercePass -> fieldType)) (_, fieldExpr) -> do
        let fieldType' = fieldType & transform \case
                TVar tv | Just tv' <- lookup tv polyTypes -> TVar tv'
                x -> x
        fieldExpr' <- check fieldExpr
        exprTy <- getType' fieldExpr'
        
        tellLI li [exprTy :~ fieldType']

        pure (name, fieldExpr')

    pure (StructConstruct (Ext (coercePass structDef, retTy)) li structName fields')

check (StructAccess possibleStructs li sexpr field) = do
    sexpr' <- check sexpr
    retTy <- freshTV KStar

    sexprTy <- getType' sexpr'

    let structTys = map (coercePass . view structType) (toList possibleStructs) 

    tellLI li [OneOf sexprTy structTys]
    -- Codegen needs the correct StructDef, but we don't know that until the constraint solver is done
    pure (StructAccess (Ext (coercePass possibleStructs, sexprTy, retTy)) li sexpr' field)
check (ExprX x _) = absurd x

typecheck :: Members '[Error TypeError, Fresh Text QualifiedName, State TCState, Dump [TConstraint], Output Log] r 
          => Module Typecheck 
          -> Sem r (Module NextPass)
typecheck (Module (Ext deps) mname sts) = do
    (constraints, sts') <- runWriterAssocR $ runFreshM freshenTV $ traverse checkStmnt sts
    dump constraints
    subst <- solve mempty constraints
    pure $ Module (Ext deps) mname (applySubst subst sts')
        where
            freshenTV :: forall r. Members '[Fresh Text QualifiedName] r => TVar NextPass -> Sem r (TVar NextPass)
            freshenTV (MkTVar x k) = MkTVar <$> freshVar (originalName x) <*> pure k 

solve :: Members '[Error TypeError, Output Log] r => Substitution -> [TConstraint] -> Sem r Substitution
solve s (((applySubst s -> t1) :~ (applySubst s -> t2), li):cs) = do
    log LogDebugVerbose $ "Solving constraint: " <> ppConstraint (t1 :~ t2)
    s' <- runReader li $ unify t1 t2
    solve (s <> s') cs 

solve s ((OneOf (applySubst s -> t1) (map (applySubst s) -> ts), li):cs) = do
    (rights <$> traverse (\t -> fmap (,t) <$> runError (runReader li (unify t1 t))) ts) >>= \case
        [(s', _)] -> solve (s <> s') cs
        []  -> throw $ NoStructsForType li t1
        ts  -> throw $ AmbiguousStructAccess li t1 (map snd ts)

solve s [] = pure s

unify :: Members '[Reader LexInfo, Error TypeError, Output Log] r => Type -> Type -> Sem r Substitution
unify t1 t2 = do
    s <- unify' t1 t2
    log LogDebugVeryVerbose $ "Unified: " <> ppType t1 <> " ~ " <> ppType t2 <> "\n    -> " <> show s 
    pure s

unify' :: Members '[Reader LexInfo, Error TypeError, Output Log] r => Type -> Type -> Sem r Substitution
unify' t1@(TCon c1 _k1) t2@(TCon c2 _k2)
    | c1 == c2 = pure mempty
    | otherwise = throwLI \li -> DifferentConstructor li t1 t2
unify' (TVar tv) t2              = bind tv t2
unify' t1 (TVar tv)              = bind tv t1
unify' (TApp c1 a1) (TApp c2 a2) = do
    s <- unify c1 c2
    (s <>) <$> unify (applySubst s a1) (applySubst s a2)
unify' t1@TCon{}    t2@TApp{}    = throwLI \li -> NotEnoughArgs li t1 t2
unify' t1@TApp{}    t2@TCon{}    = throwLI \li -> NotEnoughArgs li t1 t2
unify' t1@TForall{} t2           = throwLI \li -> HigherPoly li t1 t2
unify' t1           t2@TForall{} = throwLI \li -> HigherPoly li t1 t2

bind :: Members '[Reader LexInfo, Error TypeError] r => TVar NextPass -> Type -> Sem r Substitution
bind tv t
    | occurs tv t   = throwLI (\li -> Occurs li tv t)
    | TVar tv == t  = pure mempty
    | otherwise     = pure (Subst (one (tv, t)))



occurs :: TVar NextPass -> Type -> Bool
occurs _ TVar{} = False
occurs tv t = tv `elem` [tv' | TVar tv' <- universeBi t]


freshTV :: Members '[Fresh Text QualifiedName] r => Kind -> Sem r Type
freshTV k = freshVar "u" <&> \u -> TVar (MkTVar u k) 

replaceTVar :: TVar NextPass -> TVar NextPass -> Type -> Type
replaceTVar a b = transformBi \case
    a' | a == a' -> b
    c -> c

applySubst :: Data from => Substitution -> from -> from
applySubst s = transformBi \case
    TVar a' | Just t' <- lookup a' (unSubst s) -> t'
    x -> x

tellLI :: (Functor f, Members '[Writer (f (a, LexInfo))] r) => LexInfo -> f a -> Sem r ()
tellLI li xs = tell (fmap (,li) xs)

throwLI :: Members '[Reader LexInfo, Error e] r => (LexInfo -> e) -> Sem r a
throwLI e = ask >>= throw . e



ppTC :: [TConstraint] -> Text
ppTC = unlines . map (\(c, l) -> ppConstraint c <> "    @" <> show l) 

ppConstraint :: TConstraintComp -> Text
ppConstraint (t1 :~ t2)     = ppType t1 <> " ~ " <> ppType t2
ppConstraint (OneOf t1 ts)  = ppType t1 <> " ∈ {" <> T.intercalate ", " (map ppType ts) <> "}"

ppType :: Type -> Text
ppType (a :-> b)            = "(" <> ppType a <> " -> " <> ppType b <> ")"
ppType (TVar (MkTVar v _))  = show v
ppType (TCon v _)           = show v
ppType (TApp a b)           = "(" <> ppType a <> " " <> ppType b <> ")"
ppType (TForall ps t)       = "(∀" <> T.intercalate " " (map (\(MkTVar v _) -> show v) ps) <> ". " <> ppType t <> ")"

-- (forall a3. ((->) (forall a3. a3))) (forall a3. a3)
