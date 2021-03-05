{-#LANGUAGE NoImplicitPrelude, DataKinds, BlockArguments#-}
{-# LANGUAGE TypeApplications, NamedFieldPuns #-}
module Language.MCScript where

import Language.MCScript.Prelude

import Language.MCScript.Compiler as S
import Language.MCScript.Types as S

import Language.MCScript.Typechecker as TC

import Language.MCScript.MCAsm.Compiler as A
import Language.MCScript.MCAsm.Types as A

data CompilationError = CompilerError CompilerError
                      | AsmError McAsmError
                      | TypeError TypeError
                      deriving (Show, Eq)

compileFully :: Text -> Bool -> [S.Module 'Unaltered] -> Either CompilationError [CompiledModule]
compileFully nameSpace debug mods = do
    tmods <- first TypeError $ run $ runError $ evalState initialTCState $ traverse (TC.typecheckModule) mods
    asmMods <- join $ first CompilerError $ fmap (first AsmError) $ 
        run $ runError @CompilerError $ runError @McAsmError $ evalState S.initialCompileState $ traverse S.compile tmods
    first AsmError $ run $ runReader (CompEnv debug nameSpace) $ evalState A.initialCompState $ runError $ A.compile asmMods

compileToFunctionsAtPath :: FilePath -> Text -> Bool -> [S.Module 'Unaltered] -> Either CompilationError (IO ())
compileToFunctionsAtPath path nameSpace debug mods = compileFully nameSpace debug mods 
    <&> traverse_ \CompiledModule{compModName, compModInstructions} ->
        writeFileText (path </> toString compModName) compModInstructions
        
