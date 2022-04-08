module Cobble.Build where

import Cobble.Prelude hiding (writeFile)

import Cobble.Util.Polysemy.FileSystem
import Cobble.Util.Bitraversable

import Data.Yaml as Y
import Data.Aeson as A

import System.Directory as D
import System.FilePath

import Cobble.Types

import Cobble.Parser
import Cobble.Prelude.Parser (parse, ParseError)
import Cobble.Parser.Tokenizer

import Cobble.ModuleSolver qualified as S

data BuildError = ProjectParseError ParseException
                | ModuleNotFound Text
                | BuildLexicalError LexicalError
                | BuildParseError ParseError
                | BuildModuleSolverError S.ModuleError
                deriving (Show, Generic)

data BuildOpts = BuildOpts {
    projectFile :: FilePath
} deriving (Show, Eq, Generic, Data)

createMakefile :: Members '[FileSystem FilePath Text, Error BuildError, Embed IO] r => BuildOpts -> Sem r FilePath
createMakefile buildOpts@BuildOpts{projectFile} = do
    projectOpts@ProjectOpts{projectName, sourceDir, outDir} <- embed (decodeFileEither projectFile) >>= \case
        Right x -> pure x
        Left err -> throw (ProjectParseError err)
    let baseDirectory = takeDirectory projectFile

    fileDependencies <- findFileDependencies sourceDir

    writeFile (baseDirectory </> "Makefile") =<< renderMakefile buildOpts projectOpts fileDependencies
    pure baseDirectory

findFileDependencies :: Members '[Embed IO, Error BuildError] r 
                     => FilePath 
                     -> Sem r (Seq (FilePath, Seq FilePath))
findFileDependencies sourceDir = do
    sourceFiles <- embed $ findFilesRecursive ((==".cb") . takeExtension) sourceDir
    
    filesWithImports <- traverse (\file -> (file,) <$> (parseImports (toText file) =<< readFileText (sourceDir </> file))) sourceFiles

    let moduleMap :: Map Text FilePath = fromList $ toList $ map (\(fileName, (modName, _)) -> (modName, fileName)) filesWithImports

    forM filesWithImports $ secondM $ \(_, depMods) -> forM depMods \depMod -> case lookup depMod moduleMap of
        Nothing -> throw $ ModuleNotFound depMod
        Just fp -> pure fp



parseImports :: Members '[Error BuildError] r => Text -> Text -> Sem r (Text, Seq Text)
--                                                                      ^module name ^imports (as modules)
parseImports path code = do
    tokens <- mapError BuildLexicalError $ tokenize path code
    ast@(Module _ modName sts) <- mapError BuildParseError $ fromEither $ parse module_ (toString path) tokens
    (imports, _) <- mapError BuildModuleSolverError $ S.collectImports sts
    pure (modName, imports)

findFilesRecursive :: (FilePath -> Bool) -> FilePath -> IO (Seq FilePath)
findFilesRecursive pred = go ""
    where
        go takenPath root = D.doesDirectoryExist root >>= \case
            True -> do
                files <- listDirectory root
                fold <$> traverse (\file -> go (takenPath </> file) (root </> file)) files
            False -> do
                if pred root then pure [takenPath] else pure []

renderMakefile :: BuildOpts -> ProjectOpts -> Seq (FilePath, Seq FilePath) -> Sem r Text
renderMakefile BuildOpts{projectFile} ProjectOpts{projectName, sourceDir, outDir, compiler, compilerOpts} deps
    = pure $ all <> "\n" <> rules <> "\n" <> clean
    where
        clean :: Text
        clean = unlines [
                ".PHONY: clean"
            ,   "clean:"
            ,   "\trm -r " <> toText outDir
            ,   ""
            ]
        all = unlines [
                ".PHONY: all"
            ,   "all: " <> unwords (toList $ map (\(path,_) -> sigFileAt path) deps)
                -- TODO: add racket link command
            ]
        rules = unlines $ toList deps <&> \(file, deps) -> unlines [
                sigFileAt file <> ": " <> unwords (sourceFileAt file : toList (map sigFileAt deps))
            ,   "\tmkdir -p " <> toText (takeDirectory (outDir </> file))
            ,   "\t" <> compiler <> " " <> compilerOpts <> " " <> unwords (sourceFileAt file : toList (map sigFileAt deps))
                    <> " -o " <> sigFileAt file
            ]

        sigFileAt path = toText $ outDir </> dropExtension path <.> "cbi"
        sourceFileAt path = toText $ sourceDir </> path



data ProjectOpts = ProjectOpts {
    projectName :: Text
,   sourceDir :: FilePath
,   outDir :: FilePath
,   compiler :: Text
,   compilerOpts :: Text
} deriving (Show, Eq, Generic, Data)

instance FromJSON ProjectOpts where
    parseJSON = withObject "Project" $ \p -> ProjectOpts
        <$> p .: "name"
        <*> p .: "source-dir"
        <*> p .: "out-dir"
        <*> (p .: "compiler" <|> pure "cobble compile")
        <*> (p .: "compiler-options" <|> pure "")

