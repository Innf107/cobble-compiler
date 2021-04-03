module Language.Cobble where

import Language.Cobble.Prelude hiding ((<.>), readFile)

import Language.Cobble.Compiler as S
import Language.Cobble.Types as S
import Language.Cobble.Parser.Tokenizer as S
import Language.Cobble.Parser as S
import Language.Cobble.Qualifier as S
import Language.Cobble.Packager
import Language.Cobble.ModuleSolver
import Language.Cobble.Util.Polysemy.Time
import Language.Cobble.Util.Polysemy.FileSystem
import Language.Cobble.Util

import Language.Cobble.Prelude.Parser (ParseError, parse)

import Language.Cobble.Typechecker as TC

import Language.Cobble.MCAsm.Compiler as A
import Language.Cobble.MCAsm.Types as A

import Language.Cobble.MCAsm.McFunction

import Data.List qualified as L
import System.FilePath qualified as FP

data CompilationError = LexError LexicalError
                      | ParseError ParseError
                      | QualificationError QualificationError
                      | CompilerError CompilerError
                      | AsmError McAsmError
                      | TypeError TypeError
                      | ModuleError ModuleError
                      deriving (Show, Eq)


type ControllerC r = Members '[Reader CompileOpts, Error CompilationError, FileSystem FilePath Text] r

runControllerC :: Sem '[Reader CompileOpts, Error CompilationError, FileSystem FilePath Text] a 
               -> IO (Either CompilationError a)
runControllerC = undefined

-- TODO
data CompileOpts = CompileOpts {
      name::Text
    , fileName::Text
    , debug::Bool
    }

{-
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
-}
compileAll :: (ControllerC r) => [FilePath] -> Sem r [CompiledModule]
compileAll files = do
    tokens <- traverse (\fn -> mapError LexError $ tokenizeError (toText fn) =<< readFile fn) files
    asts   <- traverse (\(ts, n) -> mapError ParseError $ fromEither $ parse (module_ (getModName n)) n ts) (zip tokens files)

    orderedMods <- mapError ModuleError $ findCompilationOrder asts

    evalState mempty $ map fst <$> traverse compileWithSig orderedMods

compileWithSig :: (ControllerC r, Member (State (Map (S.Name 'ResolveImports) ModSig)) r)
               => S.Module 'ResolveImports
               -> Sem r (CompiledModule, ModSig)
compileWithSig mods = do
    imods <- mapError QualificationError $ traverse resolveImports mods
    undefined 

getModName :: FilePath -> Text
getModName = toText . FP.dropExtension . L.last . segments


