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
    C.Let x e c       -> do
        (eTLs, eLocs, eExp) <- compileExpr e
        (cTLs, cC) <- compileC c
        pure (eTLs <> cTLs, withLocals eLocs (T.Let x eExp cC))
    C.App3 f v1 v2    -> freshen ("f", "v1", "v2") >>= \(f', v1', v2') ->
        (\(fTLs, fLocs, fExp) (v1TLs, v1Locs, v1Exp) (v2TLs, v2Locs, v2Exp) -> 
            (fTLs <> v1TLs <> v2TLs, withLocals (fLocs <> v1Locs <> v2Locs <> [(f', fExp), (v1', v1Exp), (v2', v2Exp)]) (T.App f' [v1', v2'])))
        <$> compileVal f
        <*> compileVal v1
        <*> compileVal v2 
    C.App2 f v        -> freshen ("f", "v") >>= \(f', v') ->
        (\(fTLs, fLocs, fExp) (vTLs, vLocs, vExp) -> 
            (fTLs <> vTLs, withLocals (fLocs <> vLocs <> [(f', fExp), (v', vExp)]) (T.App f' [v'])))
        <$> compileVal f
        <*> compileVal v 
    where
        withLocals :: [LocalBinding] -> TLC -> TLC
        withLocals = flip (foldr (\(x, e) r -> T.Let x e r))

compileExpr :: (Members '[State Int] r) => CPSExpr -> Sem r ([TopLevelBinding], [LocalBinding], TLExp)
compileExpr = \case
    C.Val v         -> compileVal v
    C.Tuple vs      -> do
        vs' <- traverse compileVal vs
        let (vsTLs, vsLocs, vsExps) = vs' & foldr (\(tls, locs, e) (tls', locs', es') -> (tls <> tls', locs <> locs', e : es')) ([], [], [])
        vsNames <- traverse (\_ -> freshVar "v") vsExps
        pure (vsTLs, vsLocs <> zip vsNames vsExps, T.Tuple vsNames)
    C.Select n v    -> do
        (vTLs, vLocs, vExp) <- compileVal v
        t' <- freshVar "t"
        pure (vTLs, vLocs <> [(t', vExp)], T.Select n t')

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
            ys = toList $ freeVars c


-- λx. λy. (add x) y
-- λx k'. k' (λy k'. (λ_f. (λ_v. f v (λ_f. (λ_v. f v k') y)) x) add)
-- λx k'. k' (λy k'. add x (λ_f. add y k'))

-- λx k'. k' (λy k'. let f = add in let v = x in f v (λ_f. let v = y in f v k'))

-- λx k.0. k.0 (λy k.1. add x (λ_f.2. f.2 y k.1))

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






