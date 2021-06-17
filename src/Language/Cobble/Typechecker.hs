module Language.Cobble.Typechecker where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert
import Language.Cobble.Util.Bitraversable
import Language.Cobble.Types
import Language.Cobble.Types.Lens
import Language.Cobble.Shared

type NextPass = 'Codegen

data TypeError = VarDoesNotExist LexInfo (Name NextPass)
               --  | FunctionDoesNotExist LexInfo (Name NextPass) -- Unused since functions are now treated as plain exprs
               | WrongFunArgs LexInfo (Maybe (Name NextPass)) [Type NextPass] [Type NextPass]
               | TooManyFunArgs Natural (Type NextPass)
--                                       ^ expected
               | TooManyAppliedArgs Natural (Type NextPass)
               | WrongReturnType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                         ^ expected
               | WrongDeclType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                  ^ expected
               | WrongAssignType LexInfo (Name NextPass) (Type NextPass) (Type NextPass)
--                                    ^ expected
               | StructAccessOnNonStructType LexInfo (Type NextPass) UnqualifiedName
               | StructDoesNotContainField LexInfo (Type NextPass) UnqualifiedName
--                                                    ^ Type name     ^ field
               | WrongIfType LexInfo (Type NextPass)
               | WrongRecordConstructionType LexInfo (Name Typecheck) (Type NextPass) (Type NextPass)
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

getVarType :: (Members '[State TCState, Error TypeError] r) => LexInfo -> Name 'Typecheck -> Sem r (Type NextPass)
getVarType l varName = gets varTypes <&> lookup varName >>= \case
    Nothing -> throw (VarDoesNotExist l varName)
    Just t -> pure t


insertVarType :: (Members '[State TCState] r) => Name NextPass -> Type NextPass -> Sem r ()
insertVarType varName t = void $ modify (\s -> s{varTypes=varTypes s & insert varName t})

typecheckModule :: (TypecheckC r, Members '[Error Panic] r) => Module 'Typecheck -> Sem r (Module NextPass)
typecheckModule (Module (Ext deps) mname instrs) = Module (Ext deps) mname <$> traverse typecheck instrs

typecheck :: (TypecheckC r, Members '[Error Panic] r) => Statement Typecheck -> Sem r (Statement NextPass)
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
    Def IgnoreExt l (Decl IgnoreExt name (Ext ps) body) ty -> do
        insertVarType name (coercePass ty)
        case splitFunType (genericLength ps) (coercePass ty) of
            Nothing -> throw $ TooManyFunArgs (genericLength ps) (coercePass ty)
            Just (ptys, retTy) -> do
                zipWithM_ (insertVarType) ps ptys
                body' <- tcExpr body
                if (getType body' == retTy)
                then pure $ Def IgnoreExt l (Decl (Ext retTy) name (Ext $ zip ps ptys) body') (coercePass ty)
                else throw (WrongReturnType l name retTy (getType body'))
        
    DefStruct IgnoreExt l name (conv -> fields) -> pure $ DefStruct IgnoreExt l name fields -- TODO: Add to state map -- or not? (The qualifier does this already right?)
    Import IgnoreExt l modName -> pure $ Import IgnoreExt l modName
    StatementX x _l -> case x of

tcDecl :: (Members '[Error Panic, Error TypeError, State TCState] r) => Decl 'Typecheck -> Sem r (Decl NextPass)
tcDecl (Decl IgnoreExt vname (Ext []) expr) = do
    expr' <- tcExpr expr
    insertVarType vname (getType expr')
    pure (Decl (Ext (getType expr')) vname (Ext []) expr')
tcDecl (Decl IgnoreExt vname ps _expr) = panic' "Local functions are not possible yet. This is *NOT* a bug" [show vname, show ps]

tcExpr :: (Members '[Error Panic, Error TypeError, State TCState] r) => Expr 'Typecheck -> Sem r (Expr NextPass)
tcExpr = \case
    IntLit IgnoreExt l x -> pure $ IntLit IgnoreExt l x
    UnitLit l -> pure $ UnitLit l
    -- FloatLit x -> pure (FloatLit x, FloatT)
    FCall IgnoreExt l f exprs -> do
        f' <- tcExpr f
        (fargs, retT) <- maybe (throw (TooManyAppliedArgs (fromIntegral (length (exprs))) (getType f'))) pure
            $ splitFunType (fromIntegral (length (exprs))) (getType f')
        
        exprs' <- traverse tcExpr exprs
        
        let exprTypes = fmap getType exprs'

        if (toList exprTypes == fargs)
        then pure $ FCall (Ext retT) l f' exprs'
        else throw $ WrongFunArgs l (tryGetFunName f) fargs (toList exprTypes)
    If x l c th el -> do
        c' <- tcExpr c
        when (getType c' /= boolT) $ throw $ WrongIfType l (getType c')
        th' <- tcExpr th
        el' <- tcExpr el
        if (getType th' == getType el')
        then pure (If (coerce x) l c' th' el')
        else throw $ DifferentIfETypes l (getType th') (getType el')
    Var IgnoreExt l vname -> (\t -> Var (Ext t) l vname) <$> getVarType l vname
    Let IgnoreExt li d body -> Let IgnoreExt li
        <$> tcDecl d
        <*> tcExpr body
    StructConstruct def li cname fields -> do
        fields' <- traverse (rightM tcExpr) fields
        -- we can assume that the fields are all present and in the same order as in the struct definition
        zipWithM_ (\(n, e) (_, coercePass -> t) -> when (getType e /= t) (throw (WrongRecordConstructionType li n t (getType e)))) fields' (view structFields def)
        let t = TCon cname KStar --TODO?
        pure $ StructConstruct (Ext (coercePass def, t)) li cname fields'
    StructAccess possibleStructs li strEx fname -> do
        strEx' <- tcExpr strEx
        let ty = getType strEx'
        case ty of
            TCon tyName _ -> do
                structDef <- note (StructAccessOnNonStructType li ty fname) $ lookup tyName possibleStructs
                structFieldType <- note (StructDoesNotContainField li ty fname) $ preview (unqualifiedFieldType fname) structDef
                pure $ StructAccess (Ext (coercePass @_ @_ @Typecheck @Codegen structDef, coercePass structFieldType)) li strEx' fname
            _ -> throw (StructAccessOnNonStructType li ty fname)
    ExprX x _l -> absurd x

splitFunType :: Natural -> Type NextPass -> Maybe ([Type NextPass], Type NextPass)
splitFunType 0 t = Just ([], t)
splitFunType argCount (t :-> ts) = splitFunType (argCount - 1) ts <&> \(as, ret) -> (t:as, ret)
splitFunType _ _ = Nothing

tryGetFunName :: Expr 'Typecheck -> Maybe (Name 'Typecheck)
tryGetFunName (Var IgnoreExt _l n) = Just n
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



