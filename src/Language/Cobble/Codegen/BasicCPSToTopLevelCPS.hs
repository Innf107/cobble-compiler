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
compile c = compileC c <&> \(bindings, cmd) -> foldr (\(fname, args, b) r -> T.LetF fname args b r) (T.C cmd) bindings

compileC :: (Members '[State Int] r) => CPS -> Sem r ([TopLevelBinding], TLC)
compileC = \case
    C.Let x e c -> do
        (eTLs, eLocs, eExp) <- compileExpr e
        (cTLs, cC) <- compileC c
        pure (eTLs <> cTLs, withLocals eLocs (T.Let x eExp cC))
    C.App3 f v1 v2 -> 
        (\(fTLs, fLocs, (fBindings, f')) (v1TLs, v1Locs, (v1Bindings, v1')) (v2TLs, v2Locs, (v2Bindings, v2')) -> 
            (
                fTLs <> v1TLs <> v2TLs 
            ,   withLocals (fLocs <> v1Locs <> v2Locs <> fBindings <> v1Bindings <> v2Bindings) (T.App f' [v1', v2']))
            )
        <$> (traverseOf _3 asVar =<< compileVal f)
        <*> (traverseOf _3 asVar =<< compileVal v1)
        <*> (traverseOf _3 asVar =<< compileVal v2) 
    C.App2 f v -> 
        (\(fTLs, fLocs, (fBindings, f')) (vTLs, vLocs, (vBindings, v')) -> 
            (
                fTLs <> vTLs
            ,   withLocals (fLocs <> vLocs <> fBindings <> vBindings) (T.App f' [v'])))
        <$> (traverseOf _3 asVar =<< compileVal f)
        <*> (traverseOf _3 asVar =<< compileVal v)
    where
        withLocals :: [LocalBinding] -> TLC -> TLC
        withLocals = flip (foldr (\(x, e) r -> T.Let x e r))

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
compileLambda xs c = freshen ("f", "s", "env", "closure") >>= \(f', s', env', closure') -> do
    (fs, c') <- compileC c
    pure (
            (f', s' : xs, ifoldr (\i x r -> T.Let x (T.Select i s') r) c' ys) : fs
        ,   [(env', T.Tuple ys), (closure', T.Tuple [env', f'])]
        ,   T.Var closure'
        )
        where
            ys = toList $ (freeVars c \\ fromList xs)

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






