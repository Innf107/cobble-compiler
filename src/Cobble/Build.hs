module Cobble.Build where

import Cobble.Prelude hiding (writeFile)

import Cobble.Util.Polysemy.FileSystem
import Cobble.Util.Bitraversable
import Cobble.Util

import Data.Yaml as Y
import Data.Aeson as A

import System.Directory as D
import System.FilePath

import Cobble.Types

import Cobble.Parser
import Cobble.Prelude.Parser (parse, ParseError)
import Cobble.Parser.Tokenizer hiding ((|>))

import Cobble.ModuleSolver qualified as S

data BuildError = ProjectParseError ParseException
                | DependencyProjectParseError FilePath ParseException
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
    projectOpts@ProjectOpts{projectName, sourceDir, dependencies} <- embed (decodeFileEither projectFile) >>= \case
        Right x -> pure x
        Left err -> throw (ProjectParseError err)
    let baseDirectory = takeDirectory projectFile
    dependencyDirs <- traverse (importPackageDep baseDirectory) dependencies

    fileDependencies <- findFileDependencies (dependencyDirs |> sourceDir)

    writeFile (baseDirectory </> "Makefile") =<< renderMakefile buildOpts projectOpts fileDependencies
    pure baseDirectory

importPackageDep :: Members '[Error BuildError, Embed IO] r
                 => FilePath 
                 -> FilePath 
                 -> Sem r FilePath
importPackageDep baseDirectory dep = do
    ProjectOpts{sourceDir, projectName=dependencyName} <- embed (decodeFileEither (baseDirectory </> dep </> "cobble.yaml")) >>= \case
        Right x -> pure x
        Left err -> throw (DependencyProjectParseError dep err)
    
    let depSourceDir = baseDirectory </> dep </> sourceDir

    sourceFiles <- embed $ listDirectory depSourceDir

    let targetPath = baseDirectory </> ".cobble/dependencies" </> toString dependencyName
    embed $ createDirectoryIfMissing True targetPath

    forM_ sourceFiles \file -> embed do
        exists <- (||) <$> D.doesDirectoryExist (targetPath </> takeFileName file) <*> D.doesFileExist (targetPath </> takeFileName file)
        when (not exists) $ copyFileOrDirectory False (depSourceDir </> file) (targetPath </> takeFileName file)
    pure targetPath
    

findFileDependencies :: Members '[Embed IO, Error BuildError] r 
                     => Seq FilePath 
                     -> Sem r (Seq (Dependency, Seq Dependency))
findFileDependencies sourceDirs = do
    sourceDirsWithFiles :: Seq (FilePath, Seq FilePath) <- embed $ traverse (\x -> (x,) <$> findFilesRecursive ((==".cb") . takeExtension) x) sourceDirs
    
    moduleDependencies :: Seq (FilePath, FilePath, Text, Seq Text) <- concat <$> forM sourceDirsWithFiles \(sourceDir, files) -> forM files \file -> do
        content <- embed $ readFileText (sourceDir </> file)
        (modName, imports) <- parseImports (toText file) content
        pure (sourceDir, file, modName, imports)

    let moduleMap :: Map Text Dependency = fromList $ toList $ map (\(sourceDir, fileName, modName, _) -> (modName, MkDependency sourceDir fileName)) moduleDependencies

    forM moduleDependencies \(sourceDir, file, _modName, imports) -> do
        imports' <- forM imports \importMod -> case lookup importMod moduleMap of
            Nothing -> throw $ ModuleNotFound importMod
            Just fp -> pure fp
        pure (MkDependency sourceDir file, imports')

data Dependency = MkDependency {
        depSourceDir    :: FilePath
    ,   depRelPath      :: FilePath
    } deriving (Show, Eq, Generic, Data)

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

renderMakefile :: BuildOpts -> ProjectOpts -> Seq (Dependency, Seq Dependency) -> Sem r Text
renderMakefile BuildOpts{projectFile} ProjectOpts{projectName, compiler, compilerOpts} deps
    = pure $ all <> "\n" <> rules <> "\n" <> clean
    where
        clean :: Text
        clean = unlines [
                ".PHONY: clean"
            ,   "clean:"
            ,   "\t-rm -r .cobble/out"
            ,   "\t-rm -r .cobble/dependencies"
            ,   ""
            ]
        all = unlines [
                ".PHONY: all"
            ,   "all: " <> unwords (toList $ map (sigFileAt . fst) deps)
                -- TODO: add racket link command
            ]
        rules = unlines $ toList deps <&> \(dep, depDependencies) -> unlines [
                sigFileAt dep <> ": " <> unwords (sourceFileAt dep : toList (map sigFileAt depDependencies))
            ,   "\tmkdir -p " <> toText (takeDirectory (toString $ outputFileAt dep))
            ,   "\t" <> compiler <> " " <> compilerOpts <> " " <> unwords (sourceFileAt dep : toList (map sigFileAt depDependencies))
                    <> " -o " <> outputFileAt dep
            ]

        outputFileAt (MkDependency{depRelPath}) = toText $ ".cobble/out" </> dropExtension depRelPath <.> "rkt" 
        sigFileAt    (MkDependency{depRelPath}) = toText $ ".cobble/out" </> dropExtension depRelPath <.> "cbi"
        sourceFileAt (MkDependency{depRelPath, depSourceDir}) = toText $ depSourceDir </> depRelPath



data ProjectOpts = ProjectOpts {
    projectName :: Text
,   sourceDir :: FilePath
,   dependencies :: Seq FilePath
,   compiler :: Text
,   compilerOpts :: Text
} deriving (Show, Eq, Generic, Data)

instance FromJSON ProjectOpts where
    parseJSON = withObject "Project" $ \p -> ProjectOpts
        <$> p .: "name"
        <*> p .: "source-dir"
        <*> p .: "dependencies"
        <*> (p .: "compiler" <|> pure "cobble compile")
        <*> (p .: "compiler-options" <|> pure "")

