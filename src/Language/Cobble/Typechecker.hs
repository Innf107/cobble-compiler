{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Typechecker where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert
import Language.Cobble.Util.Bitraversable
import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Util.Polysemy.Dump
import Language.Cobble.Types hiding (Type)
import Language.Cobble.Types qualified as C 
import Language.Cobble.Types.Lens

import qualified Data.Map as M

type NextPass = 'Codegen

type Type = C.Type NextPass

newtype TCState = TCState {
    _varTypes :: M.Map QualifiedName Type
} deriving (Show, Eq, Generic, Data, Semigroup, Monoid)
makeLenses ''TCState

data TypeError = DifferentConstructor LexInfo Type Type 
               | NotEnoughArgs LexInfo Type Type
               | Occurs LexInfo (TVar NextPass) Type 
               deriving (Show, Eq, Generic, Data)

type TConstraint = (TConstraintComp, LexInfo)

data TConstraintComp = Type :~ Type deriving (Show, Eq, Generic, Data)

newtype Substitution = Subst {unSubst :: Map (TVar NextPass) Type} 
    deriving stock   (Show, Eq, Generic, Data)

-- Not sure if this is really associative...
instance Semigroup Substitution where
    Subst s1 <> s2 = Subst $ M.filterWithKey notIdentical $ fmap (applySubst s2) s1
        where
            notIdentical tv (TVar tv') | tv == tv' = False
            notIdentical _ _ = True

instance Monoid Substitution where
    mempty = Subst mempty

lookupType :: Members '[State TCState] r => QualifiedName -> Sem r Type
lookupType v = gets (lookup v . view varTypes) <&> fromMaybe (error $ "lookupType: Typechecker cannot find variable: " <> show v)

insertType :: Members '[State TCState] r => QualifiedName -> Type -> Sem r ()
insertType v t = modify (varTypes %~ insert v t)

checkStmnt :: Members '[Writer [TConstraint], Fresh Text QualifiedName, State TCState] r => Statement Typecheck -> Sem r (Statement NextPass)
checkStmnt (Import IgnoreExt li mod) = pure $ Import IgnoreExt li mod
checkStmnt (_s@DefStruct{}) = error "checkStmnt: Typechecking records is NYI" 
checkStmnt (Def IgnoreExt li (Decl IgnoreExt f (Ext ps) e) (coercePass -> ty)) = do
    insertType f ty
    psTys <- traverse (\_ -> freshTV KStar) ps
    zipWithM_ insertType ps psTys

    e' <- check e
    tellLI li [ty :~ foldr (:->) (getType e') psTys]
    pure (Def IgnoreExt li (Decl (Ext ty) f (Ext (zip ps psTys)) e') ty)

checkStmnt (StatementX x _) = absurd x

check :: Members '[Writer [TConstraint], Fresh Text QualifiedName, State TCState] r => Expr Typecheck -> Sem r (Expr NextPass)
check (IntLit IgnoreExt li n)   = pure (IntLit IgnoreExt li n)
check (UnitLit li)              = pure (UnitLit li)
check (Var IgnoreExt li vname)  = Var . Ext <$> lookupType vname <*> pure li <*> pure vname
check (FCall IgnoreExt li f as) = do
    f' <- check f
    as' <- traverse check as

    ret <- freshTV KStar
    tellLI li [getType f' :~ (foldr (:->) ret (fmap getType as'))]
    pure (FCall (Ext (getType f')) li f' as')
check (If IgnoreExt li cond th el) = do
    cond' <- check cond
    th' <- check th
    el' <- check el
    tellLI li [getType cond' :~ boolT, getType th' :~ getType el']
    pure (If IgnoreExt li cond' th' el')
check (Let IgnoreExt li (Decl IgnoreExt f (Ext ps) e) body) = do
    fTy <- freshTV KStar
    psTys <- traverse (\_ -> freshTV KStar) ps

    insertType f fTy
    zipWithM_ insertType ps psTys
    e' <- check e
    body' <- check body

    tellLI li [fTy :~ foldr (:->) (getType e') psTys]
    pure (Let IgnoreExt li (Decl (Ext fTy) f (Ext (zip ps psTys)) e') body')
check (ExprX x _) = absurd x
check _ = error "check: Typechecking records is NYI"

typecheck :: Members '[Error TypeError, Fresh Text QualifiedName, State TCState, Dump [TConstraint]] r 
          => Module Typecheck 
          -> Sem r (Module NextPass)
typecheck (Module (Ext deps) mname sts) = do
    (constraints, sts') <- runWriterAssocR $ traverse checkStmnt sts
    dump constraints
    subst <- solve mempty constraints
    pure $ Module (Ext deps) mname (applySubst subst sts')

solve :: Members '[Error TypeError] r => Substitution -> [TConstraint] -> Sem r Substitution
solve s ((t1 :~ t2, li):cs) = do
    s' <- runReader li $ unify t1 t2
    solve (s <> s') cs 
solve s [] = pure s

unify :: Members '[Reader LexInfo, Error TypeError] r => Type -> Type -> Sem r Substitution
unify t1@(TCon c1 _k1) t2@(TCon c2 _k2)
    | c1 == c2 = pure mempty
    | otherwise = throwLI \li -> DifferentConstructor li t1 t2
unify (TVar tv) t2              = bind tv t2
unify t1 (TVar tv)              = bind tv t1
unify (TApp c1 a1) (TApp c2 a2) = do
    s <- unify c1 c2
    (s <>) <$> unify a1 a2
unify t1@TCon{} t2@TApp{}       = throwLI \li -> NotEnoughArgs li t1 t2
unify t1@TApp{} t2@TCon{}       = throwLI \li -> NotEnoughArgs li t1 t2

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


applySubst :: Data from => Substitution -> from -> from
applySubst s = transformBi \case
    TVar a' | Just t' <- lookup a' (unSubst s) -> t'
    x -> x

tellLI :: (Functor f, Members '[Writer (f (a, LexInfo))] r) => LexInfo -> f a -> Sem r ()
tellLI li xs = tell (fmap (,li) xs)

throwLI :: Members '[Reader LexInfo, Error e] r => (LexInfo -> e) -> Sem r a
throwLI e = ask >>= throw . e
