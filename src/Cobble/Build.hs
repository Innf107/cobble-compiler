{-# LANGUAGE TemplateHaskell #-}
module Cobble.Build where

import Cobble.Prelude hiding (writeFile)

import Cobble.Util.Polysemy.FileSystem
import Cobble.Util.Bitraversable
import Cobble.Util

import Data.Yaml as Y
import Data.Aeson as A
import Data.Text qualified as T
import Data.Char (isSpace)

import System.Directory as D
import System.FilePath

import Cobble.Syntax

import Cobble.Parser
import Cobble.Prelude.Parser (parse, ParseError)
import Cobble.Parser.Tokenizer hiding ((|>))

import Cobble.ModuleSolver qualified as S
import Control.Concurrent.Async (mapConcurrently)

import Cobble.Util.TH 
import Data.Graph

import Data.List qualified as List

data BuildError = ProjectParseError ParseException
                | DependencyProjectParseError FilePath ParseException
                | ModuleNotFound Text
                | BuildLexicalError LexicalError
                | BuildParseError ParseError
                | BuildModuleSolverError S.ModuleError
                | UnknownPragma Text
                deriving (Show, Generic)

data LinkError = CyclicalDependencies [FilePath]

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
        exists <- (||) 
                    <$> D.doesDirectoryExist (targetPath </> takeFileName file) 
                    <*> D.doesFileExist (targetPath </> takeFileName file)
        unless exists $ copyFileOrDirectory False (depSourceDir </> file) (targetPath </> takeFileName file)
    pure targetPath
    

findFileDependencies :: Members '[Embed IO, Error BuildError] r 
                     => Seq FilePath 
                     -> Sem r (Seq (Dependency, Seq Dependency))
findFileDependencies sourceDirs = do
    sourceDirsWithFiles :: Seq (FilePath, Seq FilePath) <- embed $ traverse (\x -> (x,) <$> findFilesRecursive ((==".cb") . takeExtension) x) sourceDirs
    
    moduleDependencies :: Seq (FilePath, FilePath, Text, Seq Text, Seq Text) <- 
        concat <$> 
            forM sourceDirsWithFiles \(sourceDir, files) -> 
            forM files \file -> do
                content <- embed $ readFileText (sourceDir </> file)
                (modName, imports) <- parseImports (toText file) content
                options <- parseOptions content
                pure (sourceDir, file, modName, imports, options)

    let moduleMap :: Map Text Dependency = 
            fromList $ toList $ map 
                (\(sourceDir, fileName, modName, _, options) -> (modName, MkDependency sourceDir fileName options)) 
                moduleDependencies

    forM moduleDependencies \(sourceDir, file, _modName, imports, options) -> do
        imports' <- forM imports \importMod -> case lookup importMod moduleMap of
            Nothing -> throw $ ModuleNotFound importMod
            Just fp -> pure fp
        pure (MkDependency sourceDir file options, imports')

data Dependency = MkDependency {
        depSourceDir    :: FilePath
    ,   depRelPath      :: FilePath
    ,   depOptions      :: Seq Text
    } deriving (Show, Eq, Ord, Generic, Data)

parseImports :: Members '[Error BuildError] r => Text -> Text -> Sem r (Text, Seq Text)
--                                                                                ^     ^ imports (as modules)
--                                                                                | module name
parseImports path code = do
    tokens <- mapError BuildLexicalError $ tokenize path code
    (Module _ modName sts) <- mapError BuildParseError $ fromEither $ parse module_ (toString path) tokens
    (imports, _) <- mapError BuildModuleSolverError $ S.collectImports sts
    pure (modName, imports)

parseOptions :: forall r. Members '[Error BuildError] r => Text -> Sem r (Seq Text)
parseOptions content = go (lines content)
    where
        go :: [Text] -> Sem r (Seq Text)
        go [] = pure []
        go (line : lines)
            | Just pragma <- T.stripPrefix "--#" (T.strip line) = 
                case T.stripPrefix "OPTIONS:" (T.strip pragma) of
                    Just optText -> 
                        let options = map T.strip (fromList (words optText)) in
                        (options <>) <$> go lines
                    Nothing ->
                        throw (UnknownPragma pragma)
            | T.all isSpace line = go lines
            | otherwise = pure []

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
renderMakefile BuildOpts{} ProjectOpts{linker, linkerOpts, compiler, compilerOpts, outFile, projectName} deps
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
            ,   "\t" <> linker <> " " <> linkerOpts <> " -o '" <> outFilePath <> "' " <> unwords (fmap outputFileAt linkOrderedDeps)
            ,   "\tchmod +x '" <> outFilePath <> "'"
            ]
        rules = unlines $ toList deps <&> \(dep, depDependencies) -> unlines [
                sigFileAt dep <> ": " <> unwords (sourceFileAt dep : toList (map sigFileAt depDependencies))
            ,   "\tmkdir -p " <> toText (takeDirectory (toString $ outputFileAt dep))
            ,   "\t" <> compiler <> " " <> compilerOpts <> " " <> unwords (toList (depOptions dep)) 
                    <> " " <> unwords (sourceFileAt dep : toList (map sigFileAt depDependencies))
                    <> " -o " <> outputFileAt dep
            ]
        linkOrderedDeps =
            let (graph, vertexToDep) = graphFromEdges' (fmap (\(x, deps) -> (x, x, toList deps)) (toList deps)) in
            List.reverse (fmap ((\(x, _, _) -> x) . vertexToDep) (topSort graph))
        outFilePath = fromMaybe projectName outFile


outputFileAt :: Dependency -> Text
outputFileAt (MkDependency{depRelPath}) = toText $ ".cobble/out" </> dropExtension depRelPath <.> "rkt" 
sigFileAt :: Dependency -> Text
sigFileAt    (MkDependency{depRelPath}) = toText $ ".cobble/out" </> dropExtension depRelPath <.> "cbi"
sourceFileAt :: Dependency -> Text
sourceFileAt (MkDependency{depRelPath, depSourceDir}) = toText $ depSourceDir </> depRelPath



data ProjectOpts = ProjectOpts {
    projectName :: Text
,   sourceDir :: FilePath
,   dependencies :: Seq FilePath
,   compiler :: Text
,   compilerOpts :: Text
,   linker :: Text
,   linkerOpts :: Text
,   outFile :: Maybe Text
} deriving (Show, Eq, Generic, Data)

instance FromJSON ProjectOpts where
    parseJSON = withObject "Project" $ \p -> ProjectOpts
        <$> p .: "name"
        <*> p .: "source-dir"
        <*> p .: "dependencies"
        <*> (p .: "compiler" <|> pure "cobble compile")
        <*> (p .: "compiler-options" <|> pure "")
        <*> (p .: "linker" <|> pure "cobble link")
        <*> (p .: "linker-options" <|> pure "")
        <*> (p .:? "out-file")



-- IMPORTANT: If rts/cobble.rkt changes, this module needs to be rebuilt!
rts :: Text
rts = $(includeFile "rts/cobble.rkt")

-- | Accepts a list of racket artifact files and 'links' them together by
-- joining them all into a single large racket file that includes the cobble runtime.
-- IMPORTANT: The order in which these paths are supplied matters!
-- The order has to be a topological sort of the dependency graph and there are currently no checks to
-- ensure this!
-- The order is only guaranteed to be correct for the link command generated by 'cobble build'.
linkFiles :: Seq FilePath -> FilePath -> IO ()
linkFiles modFiles outPath = do
    outContent <- unlines <$> mapConcurrently readFileText (toList modFiles)
    writeFileText outPath ("#!/usr/bin/env racket\n#lang racket\n" <> rts <> "\n" <> outContent)
