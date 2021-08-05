module Language.Cobble.Codegen.BasicCPSToTopLevelCPS where

import Language.Cobble.Prelude hiding (uncurried, (\\))
import Language.Cobble.Shared
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.CPS.TopLevel.Types as T
import Language.Cobble.Codegen.Common

import Data.Set ((\\))

type TopLevelBinding = (QualifiedName , [QualifiedName], TLC)

type LocalBinding = (QualifiedName, TLExp)

compile :: (Members '[State Int] r) => CPS -> Sem r TL
compile c = compileC c <&> \(bindings, cmd) -> bindings & foldr (\(fname, args, b) r -> T.LetF fname args b r) (T.C cmd)

compileC :: forall r. (Members '[State Int] r) => CPS -> Sem r ([TopLevelBinding], TLC)
compileC = \case
    C.Let x e c -> do
        (eTLs, eLocs, eExp) <- compileExpr e
        (cTLs, cC) <- compileC c
        pure (eTLs <> cTLs, withLocals eLocs (T.Let x eExp cC))
    C.App3 f v1 v2 -> do
        (fTLs,  fLocs,  (fBindings,  f'))  <- traverseOf _3 asVar =<< compileVal f
        (v1TLs, v1Locs, (v1Bindings, v1')) <- traverseOf _3 asVar =<< compileVal v1 
        (v2TLs, v2Locs, (v2Bindings, v2')) <- traverseOf _3 asVar =<< compileVal v2
        (unwrapBindings, f'', env') <- unwrapClosure f'
        pure (
                fTLs <> v1TLs <> v2TLs 
            ,   withLocals (fLocs <> v1Locs <> v2Locs <> fBindings <> v1Bindings <> v2Bindings <> unwrapBindings) 
                    (T.App f'' [env', v1', v2'])
            )
    C.App2 f v -> do
        -- Continuations do *not* need to unwrap their closure
        (fTLs,  fLocs,  (fBindings,  f'))  <- traverseOf _3 asVar =<< compileVal f
        (vTLs, v1Locs, (vBindings, v')) <- traverseOf _3 asVar =<< compileVal v 
        pure (
                fTLs <> vTLs 
            ,   withLocals (fLocs <> v1Locs <> fBindings <> vBindings) (T.App f' [v'])
            )
    where
        withLocals :: [LocalBinding] -> TLC -> TLC
        withLocals = flip (foldr (\(x, e) r -> T.Let x e r))
        unwrapClosure :: QualifiedName -> Sem r ([LocalBinding], QualifiedName, QualifiedName)
        unwrapClosure f = freshen ("f", "env") <&> \(f', env') -> ([(f', T.Select 0 f), (env', T.Select 1 f)], f', env')

compileExpr :: (Members '[State Int] r) => CPSExpr -> Sem r ([TopLevelBinding], [LocalBinding], TLExp)
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

compileVal :: (Members '[State Int] r) => CPSVal -> Sem r ([TopLevelBinding], [LocalBinding], TLExp)
compileVal = \case
    C.IntLit n      -> pure ([], [], T.IntLit n)
    C.Var v         -> pure ([], [], T.Var v)
    C.Lambda k x c  -> compileLambda [k, x] c
    C.Admin v c     -> compileLambda [v] c
    C.Halt          -> pure ([], [], T.Halt)

compileLambda :: (Members '[State Int] r) => [QualifiedName] -> CPS -> Sem r ([TopLevelBinding], [LocalBinding], TLExp)
compileLambda xs c = freshen ("f", "s", "env") >>= \(f', s', env') -> do
    (fs, c') <- compileC c
    pure (
            (f', s' : xs, ifoldr (\i x r -> T.Let x (T.Select i s') r) c' ys) : fs
        ,   [(env', T.Tuple ys)]
        ,   T.Tuple [f', env']
        )
        where
            ys = toList (freeVars c \\ fromList xs)

asVar :: (Member (State Int) r) => TLExp -> Sem r ([LocalBinding], QualifiedName)
asVar = \case
    T.Var x -> pure ([], x)
    e       -> freshVar "e" <&> \e' -> ([(e', e)], e')

asVars :: (Member (State Int) r) => [TLExp] -> Sem r ([LocalBinding], [QualifiedName])
asVars es = first concat . unzip <$> traverse asVar es
    

freeVars :: CPS -> Set QualifiedName
freeVars = \case
    C.Let v e b   -> freeVarsExpr e <> (freeVars b \\ one v)
    C.App2 f e    -> freeVarsVal f <> freeVarsVal e
    C.App3 f k e  -> freeVarsVal f <> freeVarsVal k <> freeVarsVal e

freeVarsExpr :: CPSExpr -> Set QualifiedName
freeVarsExpr = \case
    C.Val v         -> freeVarsVal v
    C.Tuple vs      -> foldMap freeVarsVal vs
    C.Select _ v    -> freeVarsVal v

freeVarsVal :: CPSVal -> Set QualifiedName
freeVarsVal = \case
    C.IntLit _          -> mempty
    C.Halt              -> mempty
    C.Var v             -> one v
    C.Lambda k1 k2 b    -> freeVars b \\ fromList [k1, k2]
    C.Admin x c         -> freeVars c \\ one x






