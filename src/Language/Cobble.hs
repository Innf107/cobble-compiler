module Language.Cobble where

import Language.Cobble.Prelude hiding ((<.>))
import System.FilePath ((<.>))
import System.Directory

import Language.Cobble.Compiler as S
import Language.Cobble.Types as S
import Language.Cobble.Parser.Tokenizer as S
import Language.Cobble.Parser as S
import Language.Cobble.Qualifier as S
import Language.Cobble.Packager
import Language.Cobble.Util.Polysemy.Time

import Language.Cobble.Prelude.Parser (ParseError, parse)

import Language.Cobble.Typechecker as TC

import Language.Cobble.MCAsm.Compiler as A
import Language.Cobble.MCAsm.Types as A

import Language.Cobble.MCAsm.McFunction

data CompilationError = LexError LexicalError
                      | ParseError ParseError
                      | QualificationError QualificationError
                      | CompilerError CompilerError
                      | AsmError McAsmError
                      | TypeError TypeError
                      deriving (Show, Eq)


-- TODO
data CompileOpts = CompileOpts {
      name::Text
    , fileName::Text
    , debug::Bool
    }

--TODO: Use Module
compileFileToDatapack :: CompileOpts -> Text -> IO (Either CompilationError LByteString)
compileFileToDatapack opts@CompileOpts{debug=debug_,name=name_} content = do
    compMods <- pure do
        toks     <- first LexError $ tokenize (fileName opts) content
        modAst   <- first ParseError $ parse (module_ name_) (toString (fileName opts)) toks
        qualAst  <- first QualificationError $ qualify modAst
        tcAst    <- first TypeError $ runModuleTypecheck qualAst
        asm      <- join $ run $ runError $ mapError CompilerError $ runError $ mapError AsmError $ evalState initialCompileState $ S.compile tcAst
        first AsmError $ run $ runReader A.CompEnv {debug=debug_,nameSpace=name_} $ runError $ evalState A.initialCompState $ A.compile [asm]
    case compMods of
        Left e -> pure (Left e)
        Right cms -> Right <$> (runM $ timeToIO $ makeDataPack (dataPackOptions name_ "Created with Cobble") cms)

compileFully :: NameSpace -> Bool -> [S.Module 'Typecheck] -> Either CompilationError [CompiledModule]
compileFully nameSpace debug mods = do
    tmods <- first TypeError $ run $ runError $ evalState initialTCState $ traverse (TC.typecheckModule) mods
    asmMods <- join $ first CompilerError $ fmap (first AsmError) $ 
        run $ runError @CompilerError $ runError @McAsmError $ evalState S.initialCompileState $ traverse S.compile tmods
    first AsmError $ run $ runReader (CompEnv debug nameSpace) $ evalState A.initialCompState $ runError $ A.compile asmMods

compileToFunctionsAtPath :: FilePath -> NameSpace -> Bool -> [S.Module 'Typecheck] -> Either CompilationError (IO ())
compileToFunctionsAtPath path nameSpace debug mods = compileFully nameSpace debug mods <&> \cmods -> do 
        createDirectory (path </> toString nameSpace)
        createDirectory (path </> toString nameSpace </> "functions")
        for_ cmods \CompiledModule{compModName, compModInstructions} ->
            writeFileText (path </> toString nameSpace </> "functions" </> show compModName <.> "mcfunction") compModInstructions
        
