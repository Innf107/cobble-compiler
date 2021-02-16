{-#LANGUAGE NoImplicitPrelude, ConstraintKinds, DataKinds, LambdaCase#-}
module Language.MCScript.Compiler where

import Language.MCScript.Prelude

import Language.MCScript.Types as S
import Language.MCScript.Typechecker as S

import Language.MCScript.MCAsm.Types as A


type CompileC r = Members '[] r

compile :: (CompileC r) => S.TypedModule -> Sem r A.Module
compile (S.TypedModule modname stmnts) = A.Module modname . concat <$> traverse compileStatement stmnts

compileStatement :: (CompileC r) => S.TypedStatement -> Sem r [Instruction]
compileStatement = \case
    _ -> undefined



