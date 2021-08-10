module Language.Cobble.Codegen.TopLevelCPSToMCAsm where

import Language.Cobble.Prelude

import Language.Cobble.Shared
import Language.Cobble.Util.Polysemy.Fresh

import Language.Cobble.CPS.TopLevel.Types as T
import Language.Cobble.MCAsm.Types as A

import Language.Cobble.Codegen.PrimOps as P 

argReg :: Int -> Register
argReg i = SpecialReg ("arg" <> show i)

compile :: TL -> [Block]
compile = compile' []

compile' :: [QualifiedName] -> TL -> [Block]
compile' fs = \case 
    LetF f k ps c p -> Block f (
            Move (Reg k) (argReg 0)
        :   imap (\i x -> Move (Reg x) (argReg (i + 1))) ps
        <>  compileTLC c)
        : compile' (f:fs) p
    LetC f ps c p -> Block f (concat [
            imap (\i x -> Move (Reg x) (argReg i)) ps
        ,   compileTLC c
        ])
        : compile' (f:fs) p
    C c -> Block "main" 
            (   map (\f -> LoadFunctionAddress (Reg f) f) fs   
            <>  compileTLC c
            )
        : []

compileTLC :: TLC -> [Instruction]
compileTLC = \case
    Let x (IntLit n) c      -> MoveLit (Reg x) n : compileTLC c
    Let x (Var y) c         -> Move (Reg x) (Reg y) : compileTLC c
    Let x (Halt) c          -> MoveLit (Reg x) 0 : compileTLC c
    Let x (Tuple ys) c      -> Malloc (Reg x) (length ys)
                            :  imap (\i y -> Store (Reg x) (Reg y) i) ys
                            <> compileTLC c
    Let x (T.Select n y) c  -> A.Select (Reg x) (Reg y) n : compileTLC c 
    App f xs                -> imap (\i x -> Move (argReg i) (Reg x)) xs
                            <> [ICall (Reg f)]
    Let x (PrimOp p ys) c   -> letPrimOp x p ys <> compileTLC c 
    
letPrimOp :: QualifiedName -> PrimOp -> [QualifiedName] -> [Instruction]
letPrimOp x p = case p of
    P.True_  -> \case 
        [_] -> [MoveLit (Reg x) 1]
        ys -> wrongNumberOfArgs 1 ys
    P.False_ -> \case 
        [_] -> [MoveLit (Reg x) 0]
        ys -> wrongNumberOfArgs 1 ys
    P.Add -> binOp A.Add 
    P.Sub -> binOp A.Sub
    P.Mul -> binOp A.Mul
    P.Div -> binOp A.Div
    P.Mod -> binOp A.Mod
    P.LE  -> \case
        [y, z] -> undefined 
        ys -> wrongNumberOfArgs 2 ys
    P.SetTestScoreboardUnsafe -> undefined 
    where
        binOp :: (Register -> Register -> Instruction) -> [QualifiedName] -> [Instruction]
        binOp f = \case
            [y, z] -> [
                    Move (Reg x) (Reg y)
                ,   f (Reg x) (Reg z)
                ]
            ys -> wrongNumberOfArgs 2 ys

        wrongNumberOfArgs :: HasCallStack => Int -> [QualifiedName] -> a
        wrongNumberOfArgs expected ys = error $ "compileTLC: PrimOp '" <> show p <> "' expected "
                                    <> show expected <> " arguments, but received " <> show (length ys)





freshReg :: (Member (Fresh QualifiedName) r) => QualifiedName -> Sem r Register
freshReg = fmap Reg . freshVar
