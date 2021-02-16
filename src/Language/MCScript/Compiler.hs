{-#LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase#-}
module Language.MCScript.Compiler where

import Language.MCScript.Prelude

import Language.MCScript.Types as S
import Language.MCScript.Typechecker as S

import Language.MCScript.MCAsm.Types as A


type CompileC r = Members '[State (), Reader ()] r

compile :: (CompileC r) => S.Module 'Typed -> Sem r A.Module
compile (S.Module modname stmnts) = A.Module modname . concat <$> traverse compileStatement stmnts

compileStatement :: (CompileC r) => S.Statement 'Typed -> Sem r [Instruction]
compileStatement = \case
    _ -> undefined



