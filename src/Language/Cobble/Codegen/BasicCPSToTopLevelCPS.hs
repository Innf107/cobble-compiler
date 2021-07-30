module Language.Cobble.Codegen.BasicCPSToTopLevelCPS where

import Language.Cobble.Prelude hiding (uncurried, (\\))
import Language.Cobble.Shared
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.CPS.TopLevel.Types as T
import Language.Cobble.Codegen.Common

import Data.Set ((\\))

type TopLevelBinding = (QualifiedName, [QualifiedName], TLC)

-- compileVal :: CPSVal -> ([(QualifiedName, TLC)], (Undefined, Undefined), Undefined)
compileVal :: (Members '[State Int] r) => CPSVal -> Sem r ([TopLevelBinding], [QualifiedName])
compileVal (C.Lambda k x c) = compileLambda [k, x] c

compileLambda :: (Members '[State Int] r) => [QualifiedName] -> CPS -> Sem r ([TopLevelBinding], [QualifiedName])
compileLambda xs c = freshVar "f" >>= \f' -> freshVar "s" >>= \s' -> do
    fs <- undefined
    c' <- undefined
    pure (
            (f', s' : xs, ifoldr (\i x r -> T.Let x (T.Select i s') r) c' ys) : fs
        ,   ys
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






