module Language.Cobble.Typechecker where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert
import Language.Cobble.Types

type NextPass = 'Codegen

data TypeError = VarDoesNotExist LexInfo (Name NextPass)
               --  | FunctionDoesNotExist LexInfo (Name NextPass) -- Unused since functions are now treated as plain exprs
               | WrongFunArgs LexInfo (Maybe (Name NextPass)) [Type NextPass] [Type NextPass]
               | TooManyFunArgs Natural (Type NextPass)
--                                       ^ expected
               | WrongReturnType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                         ^ expected
               | WrongDeclType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                  ^ expected
               | WrongAssignType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                    ^ expected
               | WrongIfType LexInfo (Type NextPass)
               | WrongIfEType LexInfo (Type NextPass)
               | DifferentIfETypes LexInfo (Type NextPass) (Type NextPass)
               | WrongSetScoreboardType LexInfo Objective Text (Type NextPass)
               | CannotUnify (Type NextPass) (Type NextPass)
               | OccursCheck (Name 'Typecheck) (Type NextPass)
               | KindMismatch (Type NextPass) (Type NextPass)
               deriving (Show, Eq)

data TypeWarning = WarnIgnoredFunRetType LexInfo (Type NextPass) deriving (Show, Eq)


newtype TCState = TCState {
        varTypes::Map (Name 'Typecheck) (Type NextPass)
    } deriving stock (Show, Eq)
      deriving newtype (Semigroup, Monoid)

type TypecheckC r = Members [State TCState, Error TypeError, Output TypeWarning] r

initialTCState :: TCState
initialTCState = TCState {
        varTypes = mempty
    }

getVarType :: (TypecheckC r) => LexInfo -> Name 'Typecheck -> Sem r (Type NextPass)
getVarType l varName = gets varTypes <&> lookup varName >>= \case
    Nothing -> throw (VarDoesNotExist l varName)
    Just t -> pure t


insertVarType :: (TypecheckC r) => Name NextPass -> Type NextPass -> Sem r ()
insertVarType varName t = void $ modify (\s -> s{varTypes=varTypes s & insert varName t})

typecheckModule :: (TypecheckC r) => Module 'Typecheck -> Sem r (Module NextPass)
typecheckModule (Module deps mname instrs) = Module deps mname <$> traverse typecheck instrs

typecheck :: (TypecheckC r) => Statement Typecheck -> Sem r (Statement NextPass)
typecheck = \case
    {-
    DefFun () l fname (conv -> args) stmnts lastexpr (conv -> retT) -> do
        
        let ftype = foldr (-:>) retT (map snd args)
        insertVarType fname ftype
        
        for_ args (uncurry insertVarType)
        
        stmnts' <- traverse typecheck stmnts
        lastexpr' <- typeOf lastexpr
        if (getType lastexpr' == retT)
        then pure (DefFun () l fname args stmnts' lastexpr' retT)
        else throw (WrongReturnType l fname retT (getType lastexpr'))
    -}
    Def () l (Decl () name ps body ty) -> do
        insertVarType name (coercePass ty)
        case splitFunType (genericLength ps) (coercePass ty) of
            Nothing -> throw $ TooManyFunArgs (genericLength ps) (coercePass ty)
            Just (ptys, retTy) -> do
                zipWithM_ (insertVarType) ps ptys
                body' <- tcExpr body
                if (getType body' == retTy)
                then pure $ Def () l (Decl retTy name (zip ps ptys) body' (coercePass ty))
                else throw (WrongReturnType l name retTy (getType body'))
        
    DefStruct () l name (conv -> fields) -> pure $ DefStruct () l name fields -- TODO: Add to state map -- or not? (The qualifier does this already right?)
    Import () l modName -> pure $ Import () l modName
    StatementX x _l -> case x of

tcExpr :: (TypecheckC r) => Expr 'Typecheck -> Sem r (Expr NextPass)
tcExpr = \case
    IntLit () l x -> pure $ IntLit () l x
    -- FloatLit x -> pure (FloatLit x, FloatT)
    FCall () l f exprs -> do
        f' <- tcExpr f
        (fargs, retT) <- maybe (throw (TooManyFunArgs (fromIntegral (length (exprs))) (getType f'))) pure -- TODO: Wrong error message D:
            $ splitFunType (fromIntegral (length (exprs))) (getType f')
        
        exprs' <- traverse tcExpr exprs
        
        let exprTypes = fmap getType exprs'

        if (toList exprTypes == fargs)
        then pure $ FCall retT l f' exprs'
        else throw $ WrongFunArgs l (tryGetFunName f) fargs (toList exprTypes)
    If x l c th el -> do
        c' <- tcExpr c
        when (getType c' /= boolT) $ throw $ WrongIfEType l (getType c')
        th' <- tcExpr th
        el' <- tcExpr el
        if (getType th' == getType el')
        then pure (If x l c' th' el')
        else throw $ DifferentIfETypes l (getType th') (getType el')
    Var () l vname -> (\x -> Var x l vname) <$> getVarType l vname
    ExprX x _l -> case x of

splitFunType :: Natural -> Type NextPass -> Maybe ([Type NextPass], Type NextPass)
splitFunType 0 t = Just ([], t)
splitFunType argCount (t :-> ts) = splitFunType (argCount - 1) ts <&> \(as, ret) -> (t:as, ret)
splitFunType _ _ = Nothing

tryGetFunName :: Expr 'Typecheck -> Maybe (Name 'Typecheck)
tryGetFunName (Var () _l n) = Just n
tryGetFunName _ = Nothing

type Subst = [(Name 'Typecheck, Type NextPass)]

(+->) :: Name 'Typecheck -> Type NextPass -> Subst
n +-> t = [(n, t)]

(@@) :: Subst -> Subst -> Subst
(@@) = undefined

tv :: Type NextPass -> [Name NextPass]
tv = undefined

mgu :: (TypecheckC r) => Type NextPass -> Type NextPass -> Sem r Subst
mgu (TVar n k) t = bindVar n k t
mgu t (TVar n k) = bindVar n k t
mgu (TCon t1 k1) (TCon t2 k2)
    | t1 == t2 && k1 == k2 = pure []
mgu (TApp l1 r1) (TApp l2 r2) = (@@) <$> mgu l1 l2 <*> mgu r1 r2
mgu t1 t2 = throw $ CannotUnify t1 t2

bindVar :: (TypecheckC r) => Name NextPass -> Kind -> Type NextPass -> Sem r Subst
bindVar u k t
    | TVar u k == t         = pure []
    | u `elem` tv t         = throw (OccursCheck u t)
    | Right k /= kind t     = throw (KindMismatch (TVar u k) t)
    | otherwise             = pure [(u, t)]


