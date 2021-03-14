{-#LANGUAGE NoImplicitPrelude, DataKinds, BlockArguments#-}
{-# LANGUAGE TypeApplications, NamedFieldPuns #-}
module Language.Cobble where

import Language.Cobble.Prelude hiding ((<.>))
import System.FilePath ((<.>))
import System.Directory

import Language.Cobble.Compiler as S
import Language.Cobble.Types as S
import Language.Cobble.Parser.Preprocessor as S
import Language.Cobble.Parser.Tokenizer as S

import Language.Cobble.Typechecker as TC

import Language.Cobble.MCAsm.Compiler as A
import Language.Cobble.MCAsm.Types as A


data CompilationError = CompilerError CompilerError
                      | AsmError McAsmError
                      | TypeError TypeError
                      deriving (Show, Eq)

compileFully :: Text -> Bool -> [S.Module 'Typecheck] -> Either CompilationError [CompiledModule]
compileFully nameSpace debug mods = do
    tmods <- first TypeError $ run $ runError $ evalState initialTCState $ traverse (TC.typecheckModule) mods
    asmMods <- join $ first CompilerError $ fmap (first AsmError) $ 
        run $ runError @CompilerError $ runError @McAsmError $ evalState S.initialCompileState $ traverse S.compile tmods
    first AsmError $ run $ runReader (CompEnv debug nameSpace) $ evalState A.initialCompState $ runError $ A.compile asmMods

compileToFunctionsAtPath :: FilePath -> NameSpace -> Bool -> [S.Module 'Typecheck] -> Either CompilationError (IO ())
compileToFunctionsAtPath path nameSpace debug mods =  compileFully nameSpace debug mods 
    <&> (createDirectory (path </> toString nameSpace) *> createDirectory (path </> toString nameSpace </> "functions") *>) 
        . traverse_ \CompiledModule{compModName, compModInstructions} ->
        writeFileText (path </> toString nameSpace </> "functions" </> show compModName <.> "mcfunction") compModInstructions
        
