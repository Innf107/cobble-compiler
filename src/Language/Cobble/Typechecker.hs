module Language.Cobble.Typechecker where

import Language.Cobble.Prelude
import Language.Cobble.Util.Convert
import Language.Cobble.Types

type NextPass = 'Codegen

data TypeError = VarDoesNotExist LexInfo (Name NextPass)
               --  | FunctionDoesNotExist LexInfo (Name NextPass) -- Unused since functions are now treated as plain exprs
               | WrongFunArgs LexInfo (Maybe (Name NextPass)) [Type NextPass] [Type NextPass]
               | NotEnoughFunArgs Natural (Type NextPass)
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
    CallFun () l f exprs -> do
        f' <- typeOf f
        (fargs, retT) <- maybe (throw (NotEnoughFunArgs (genericLength exprs) (getType f'))) pure 
            $ splitFunType (genericLength exprs) (getType f')

        exprs' <- traverse typeOf exprs
        
        let exprTypes = map getType exprs'
        
        if (exprTypes /= fargs)
            then throw $ WrongFunArgs l (tryGetFunName f) fargs exprTypes
            else do
                case retT of
                    (_ :-> _) -> output (WarnIgnoredFunRetType l retT)
                    _ -> pass
                pure (CallFun () l f' exprs')
    DefFun () l fname (conv -> args) stmnts lastexpr (conv -> retT) -> do
        
        let ftype = foldr (-:>) retT (map snd args)
        insertVarType fname ftype
        
        for_ args (uncurry insertVarType)
        
        stmnts' <- traverse typecheck stmnts
        lastexpr' <- typeOf lastexpr
        if (getType lastexpr' == retT)
        then pure (DefFun () l fname args stmnts' lastexpr' retT)
        else throw (WrongReturnType l fname retT (getType lastexpr'))
    Decl () l vname (Just (conv -> t)) expr -> do
        expr' <- typeOf expr
        if (getType expr' == t)
        then insertVarType vname t >> pure (Decl () l vname (Just t) expr')
        else throw (WrongDeclType l vname t (getType expr'))
    Decl () l vname Nothing expr -> do
        expr' <- typeOf expr
        insertVarType vname (getType expr') >> pure (Decl () l vname Nothing expr')
    Assign () l vname expr -> do
        varT <- getVarType l vname
        expr' <- typeOf expr
        if (varT == getType expr')
        then pure (Assign () l vname expr')
        else throw (WrongAssignType l vname varT (getType expr'))
    IfS ni l cond th el -> IfS ni l
        <$> (typeOf cond >>= \c -> if getType c == boolT then pure c else throw (WrongIfType l (getType c)))
        <*> traverse typecheck th
        <*> traverse (traverse typecheck) el
    While () l cond stmnts -> do
        cond' <- typeOf cond
        stmnts' <- traverse typecheck stmnts
        pure (While () l cond' stmnts')
    DefStruct () l name (conv -> fields) -> pure $ DefStruct () l name fields -- TODO: Add to state map
    SetScoreboard () l obj player ex -> do
        ex' <- typeOf ex
        if (getType @_ @'Codegen ex' /= intT)
        then throw (WrongSetScoreboardType l obj player (getType ex'))
        else pure (SetScoreboard () l obj player ex')
    LogS l segs -> pure $ LogS l $ map coercePass segs
    Import () l modName -> pure $ Import () l modName
    StatementX x _l -> case x of

typeOf :: (TypecheckC r) => Expr 'Typecheck -> Sem r (Expr NextPass)
typeOf = \case
    IntLit () l x -> pure $ IntLit () l x
    -- FloatLit x -> pure (FloatLit x, FloatT)
    BoolLit () l x -> pure $ BoolLit () l x
    UnitLit l -> pure $ UnitLit l
    FCall () l f exprs -> do
        f' <- typeOf f
        (fargs, retT) <- maybe (throw (NotEnoughFunArgs (genericLength exprs) (getType f'))) pure 
            $ splitFunType (genericLength exprs) (getType f')
        
        exprs' <- traverse typeOf exprs
        
        let exprTypes = map getType exprs'

        if (exprTypes == fargs)
        then pure $ FCall retT l f' exprs'
        else throw $ WrongFunArgs l (tryGetFunName f) fargs exprTypes
    IfE x l c th el -> do
        c' <- typeOf c
        when (getType c' /= boolT) $ throw $ WrongIfEType l (getType c')
        th' <- typeOf th
        el' <- typeOf el
        if (getType th' == getType el')
        then pure (IfE x l c' th' el')
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


