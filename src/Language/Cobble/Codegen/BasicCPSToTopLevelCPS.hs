module Language.Cobble.Codegen.BasicCPSToTopLevelCPS where

{-
IMPORTANT OPTIMIZATION: Instead of allocating expensive closures, pass the environment as extra parameters wherever possible, 
                        thus massively reducing the amount of allocations.
-}

import Language.Cobble.Prelude hiding (uncurried, (\\))
import Language.Cobble.Shared
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.CPS.TopLevel.Types as T
import Language.Cobble.Codegen.Common

import Data.Set ((\\))

--               TODO: TLBindingRecF QualifiedName QualifiedName [QualifiedName] TLC
data TopLevelBinding = TLBindingF QualifiedName QualifiedName [QualifiedName] TLC
                     | TLBindingC QualifiedName [QualifiedName] TLC

type LocalBinding = (QualifiedName, TLExp)

compile :: (Members '[State Int] r) => CPS -> Sem r TL
compile c = runReader mempty $ compileC c <&> \(bindings, cmd) -> bindings & foldr (\t r -> case t of
    TLBindingF fname k args b -> T.LetF fname k args b r
    TLBindingC fname args b   -> T.LetC fname args b r
    ) (T.C cmd)

type LetRecEnv = Map QualifiedName QualifiedName
--                   ^Function     ^Environment

compileC :: forall r. (Members '[State Int, Reader LetRecEnv] r) => CPS -> Sem r ([TopLevelBinding], TLC)
compileC = \case
    C.Let x e c -> do
        (eTLs, eLocs, eExp) <- compileExpr e
        (cTLs, cC) <- compileC c
        pure (eTLs <> cTLs, withLocals eLocs (T.Let x eExp cC))
    C.LetRec f k x e c -> do
        s' <- freshen "s"
        env' <- freshen "env"
        (lamTLs, lamLocs, lamExp) <- local (insert f s') $ compileLambdaWithNames (f, s', env') k x e
        -- LetRec is (obviously) only scoped over its own function body
        (cTLs, cC) <- compileC c
        pure (lamTLs <> cTLs, withLocals lamLocs (T.Let f lamExp cC))

    C.App3 f v1 v2 -> do
        recFuns <- ask
        case f of
            C.Var fv | Just fenv <- lookup fv recFuns -> do
                -- Recursive calls are treated specially
                (v1TLs, v1Locs, (v1Bindings, v1')) <- traverseOf _3 asVar =<< compileVal v1
                (v2TLs, v2Locs, (v2Bindings, v2')) <- traverseOf _3 asVar =<< compileVal v2
                -- In a recursive call, the environment is already known, so there is no closure to unwrap
                pure 
                    (   v1TLs <> v2TLs 
                    ,   withLocals (v1Locs <> v2Locs <> v1Bindings <> v2Bindings)
                            (T.App fv [v1', fenv, v2'])
                    ) 
            _ -> do 
                (fTLs,  fLocs,  (fBindings,  f'))  <- traverseOf _3 asVar =<< compileVal f
                (v1TLs, v1Locs, (v1Bindings, v1')) <- traverseOf _3 asVar =<< compileVal v1 
                (v2TLs, v2Locs, (v2Bindings, v2')) <- traverseOf _3 asVar =<< compileVal v2
                (unwrapBindings, f'', env') <- unwrapClosure f'
                pure (
                        fTLs <> v1TLs <> v2TLs 
                    ,   withLocals (fLocs <> v1Locs <> v2Locs <> fBindings <> v1Bindings <> v2Bindings <> unwrapBindings) 
                            (T.App f'' [v1', env', v2'])
                    )
    C.App2 f v -> do
        -- Continuations *do* actually need to unwrap their closure... whoops
        -- Continuations cannot be called recursively, since they don't even exist in the source code.
        (fTLs,  fLocs,  (fBindings,  f'))  <- traverseOf _3 asVar =<< compileVal f
        (vTLs, v1Locs, (vBindings, v')) <- traverseOf _3 asVar =<< compileVal v 
        (unwrapBindings, f'', env') <- unwrapClosure f'
        pure (
                fTLs <> vTLs 
            ,   withLocals (fLocs <> v1Locs <> fBindings <> vBindings <> unwrapBindings) (T.App f'' [env', v'])
            )
    C.If c th el -> do
        (cTLs, cLocs, (cBindings, c')) <- traverseOf _3 asVar =<< compileVal c
        (thTLs, th') <- compileC th
        (elTLs, el') <- compileC el

        (thF', thEnv') <- freshen ("th", "thenv")
        let thVars = toList $ freeVars th
        thenCont <- freshen "ths" <&> \ths' -> TLBindingC thF' [ths'] (ifoldr (\i x r -> T.Let x (T.Select i ths') r) th' thVars)
        let thEnv = (thEnv', T.Tuple thVars)

        (elF', elEnv') <- freshen ("el", "elenv")
        let elVars = toList $ freeVars th
        elCont <- freshen "els" <&> \els' -> TLBindingC elF' [els'] (ifoldr (\i x r -> T.Let x (T.Select i els') r) el' elVars)
        let elEnv = (elEnv', T.Tuple elVars)

        pure (
                cTLs <> thTLs <> elTLs <> [thenCont, elCont]
            ,   withLocals (cLocs <> cBindings) 
                    $ T.If c' 
                        (withLocals [thEnv] (T.App thF' [thEnv'])) 
                        (withLocals [elEnv] (T.App elF' [elEnv']))
            )
    where
        withLocals :: [LocalBinding] -> TLC -> TLC
        withLocals = flip (foldr (\(x, e) r -> T.Let x e r))
        unwrapClosure :: QualifiedName -> Sem r ([LocalBinding], QualifiedName, QualifiedName)
        unwrapClosure f = freshen (f, "env") <&> \(f', env') -> ([(f', T.Select 0 f), (env', T.Select 1 f)], f', env')

compileExpr :: (Members '[State Int, Reader LetRecEnv] r) => CPSExpr -> Sem r ([TopLevelBinding], [LocalBinding], TLExp)
compileExpr = \case
    C.Val v         -> compileVal v
    C.Tuple vs      -> do
        vs' <- traverse compileVal vs
        let (vsTLs, vsLocs, vsExps) = vs' & foldr (\(tls, locs, e) (tls', locs', es') -> (tls <> tls', locs <> locs', e : es')) ([], [], [])
        (vBindings, vNames) <- asVars vsExps
        pure (vsTLs, vsLocs <> vBindings, T.Tuple vNames)
    C.Select n v    -> do
        (vTLs, vLocs, vExp) <- compileVal v
        (tBindings, t') <- asVar vExp
        pure (vTLs, vLocs <> tBindings, T.Select n t')
    (C.PrimOp p vs) -> do
        vs' <- traverse compileVal vs
        let (vsTLs, vsLocs, vsExps) = vs' & foldr (\(tls, locs, e) (tls', locs', es') -> (tls <> tls', locs <> locs', e : es')) ([], [], [])
        (vBindings, vNames) <- asVars vsExps
        pure (vsTLs, vsLocs <> vBindings, T.PrimOp p vNames)

compileVal :: (Members '[State Int, Reader LetRecEnv] r) => CPSVal -> Sem r ([TopLevelBinding], [LocalBinding], TLExp)
compileVal = \case
    C.IntLit n      -> pure ([], [], T.IntLit n)
    C.Var v         -> pure ([], [], T.Var v)
    C.Lambda k x c  -> compileLambda k x c
    C.Admin v c     -> compileContinuation v c
    C.Halt          -> freshen ("h", "henv") <&> \(h', henv') -> 
                        ([], [(h', T.Halt), (henv', T.Tuple [])], T.Tuple [h', henv'])


compileLambda :: (Members '[State Int, Reader LetRecEnv] r) => QualifiedName -> QualifiedName -> CPS -> Sem r ([TopLevelBinding], [LocalBinding], TLExp)
compileLambda k x c = freshen ("f", "s", "env") >>= \ns -> compileLambdaWithNames ns k x c

compileLambdaWithNames :: (Members '[State Int, Reader LetRecEnv] r) => (QualifiedName, QualifiedName, QualifiedName) -> QualifiedName -> QualifiedName -> CPS -> Sem r ([TopLevelBinding], [LocalBinding], TLExp)
compileLambdaWithNames (f', s', env') k x c = do
    (fs, c') <- compileC c
    pure (
            TLBindingF f' k [s', x] (ifoldr (\i x r -> T.Let x (T.Select i s') r) c' ys) : fs
        ,   [(env', T.Tuple ys)]
        ,   T.Tuple [f', env']
        )
        where
            ys = toList (freeVars c \\ fromList [k, x])

compileContinuation :: (Members '[State Int, Reader LetRecEnv] r) => QualifiedName -> CPS -> Sem r ([TopLevelBinding], [LocalBinding], TLExp)
compileContinuation x c = freshen ("f", "s", "env") >>= \(f', s', env') -> do
    (fs, c') <- compileC c
    pure (
            TLBindingC f' [s', x] (ifoldr (\i x r -> T.Let x (T.Select i s') r) c' ys) : fs
        ,   [(env', T.Tuple ys)]
        ,   T.Tuple [f', env']
        )
        where
            ys = toList (freeVars c \\ fromList [x])

asVar :: (Member (State Int) r) => TLExp -> Sem r ([LocalBinding], QualifiedName)
asVar = \case
    T.Var x -> pure ([], x)
    e       -> freshVar "e" <&> \e' -> ([(e', e)], e')

asVars :: (Member (State Int) r) => [TLExp] -> Sem r ([LocalBinding], [QualifiedName])
asVars es = first concat . unzip <$> traverse asVar es
    

freeVars :: CPS -> Set QualifiedName
freeVars = \case
    C.Let v e b   -> freeVarsExpr e <> (freeVars b \\ one v)
    C.LetRec f k x e b-> (freeVars e <> freeVars b) \\ fromList [f, k, x]
    C.App2 f e    -> freeVarsVal f <> freeVarsVal e
    C.App3 f k e  -> freeVarsVal f <> freeVarsVal k <> freeVarsVal e
    C.If c th el  -> freeVarsVal c <> freeVars th <> freeVars el 

freeVarsExpr :: CPSExpr -> Set QualifiedName
freeVarsExpr = \case
    C.Val v         -> freeVarsVal v
    C.Tuple vs      -> foldMap freeVarsVal vs
    C.Select _ v    -> freeVarsVal v
    C.PrimOp _ vs     -> foldMap freeVarsVal vs

freeVarsVal :: CPSVal -> Set QualifiedName
freeVarsVal = \case
    C.IntLit _          -> mempty
    C.Halt              -> mempty
    C.Var v             -> one v
    C.Lambda k1 k2 b    -> freeVars b \\ fromList [k1, k2]
    C.Admin x c         -> freeVars c \\ one x






