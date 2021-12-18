module Language.Cobble.Codegen.LCToBasicCPS where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Types.QualifiedName
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.Codegen.Common

import Data.List ((!!))

compile :: (Members '[Fresh Text QualifiedName] r) => LCExpr -> Sem r CPS
compile e = compileExpr e Halt

compileExpr :: (Members '[Fresh Text QualifiedName] r) => LCExpr -> CPSVal -> Sem r CPS
compileExpr = flip \kval -> let k = staticCont kval in \case
    L.Var v             -> pure $ k (C.Var v)
    L.IntLit n          -> pure $ k (C.IntLit n)
    L.Lambda x e        -> freshVar "k" >>= \k' -> k . C.Lambda k' x <$> compileExpr e (C.Var k')
    L.Let v e b         -> compileExpr e . C.Admin v =<< compileExpr b kval 
    L.LetRec f x e b    -> freshVar "k" >>= \k' -> C.LetRec f k' x 
        <$> compileExpr e (C.Var k')  
        <*> compileExpr b kval 
    L.App f a           -> freshVar "f" >>=  \f' -> freshVar "v" >>= \v' ->
        compileExpr f =<< C.Admin f' <$> compileExpr a (C.Admin v' (C.App3 (C.Var f') kval (C.Var v')))
    L.Tuple es          -> freshVar "t" >>= \t' -> do
        vars <- traverse freshVar $ map (("x" <>) . show) (indexes es)
        foldrM (\(v, e) r -> compileExpr e (C.Admin v r))
            (C.Let t' (C.Tuple (map C.Var vars)) (k (C.Var t')))
            (zip vars es)
    L.Select n e        -> freshVar "t" >>= \t' -> freshVar "y" >>= \y' -> compileExpr e (C.Admin t' (C.Let y' (C.Select n (C.Var t')) (k (C.Var y'))))
    L.Switch _i _branches _def -> error "LCToBasicCPS: Switch codegen NYI"
    L.PrimOp p es       -> freshVar "p" >>= \p' -> do
        vars <- traverse freshVar $ map (("x" <>) . show @Text) (indexes es)
        foldrM (\(i, e) r -> compileExpr e (C.Admin (vars !! i) r))
            (C.Let p' (C.PrimOp p (map C.Var vars)) (k (C.Var p')))
            (zipIndex es)
    --                            We bind kval as k' so the (potentially large) continuation is not (yet) duplicated.
    L.If c th el -> freshVar2 "c" "k" >>= \(c', k') -> C.Let k' (Val kval) <$> (compileExpr c =<< ((.) (C.Admin c') . C.If (C.Var c') 
                    <$> compileExpr th (C.Var k')
                    <*> compileExpr el (C.Var k')))
    L.Fail msg -> pure $ C.Fail msg


reduceAdmin :: CPS -> CPS
reduceAdmin = rewriteBi (betaLetReduce <<|>> etaReduce)
    where
        betaLetReduce :: CPS -> Maybe CPS
        betaLetReduce = \case
--          (\_x. e) y
            C.App2 (C.Admin x e) y
                | C.Var _ <- y                                      -> Just $ replaceVar x y e
                --  | length [() | C.Var v <- universeBi e, v == x] <= 1 -> Just $ replaceVar x y e
                -- TODO^: Should inlining be done in a different step?
                -- TODO: This might change evaluation order but in a purely functional language that is not important
                -- TODO: (IO might need some special cases here)
                | otherwise                                          -> Just $ C.Let x (C.Val y) e
            _                                                        -> Nothing

        etaReduce :: CPS -> Maybe CPS
        etaReduce c = case [() | C.Admin x (C.App2 _k (C.Var x')) <- universeBi c, x == x'] of
            [] -> Nothing
            _  -> Just $ c & transformBi \case
                C.Admin x (C.App2 k (C.Var x')) | x == x' -> k
                x                                         -> x

        replaceVar :: QualifiedName -> CPSVal -> CPS -> CPS
        replaceVar v e = transformBi \case
            (C.Var v') | v == v' -> e
            x -> x
                        
staticCont :: CPSVal -> (CPSVal -> CPS)
staticCont = C.App2
