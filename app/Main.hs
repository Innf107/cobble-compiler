{-#LANGUAGE NoImplicitPrelude, ScopedTypeVariables, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
module Main where

import Language.Cobble.Prelude hiding (argument)
import Language.Cobble

import Options.Applicative

main :: IO ()
main = runCobble =<< execParser (info (mainOpts <**> helper) mainInfo)
    where
        mainOpts = hsubparser (
              command "compile" (Compile <$> info compileOpts (progDesc "Compile a source fileto a datapack directly"))
            ) 
        mainInfo = idm

cobbleInfo :: InfoMod CobbleAction
cobbleInfo = mempty

runCobble :: CobbleAction -> IO ()
runCobble = \case
    Compile co -> runCompile co

runCompile :: CompileCmdOpts -> IO ()
runCompile CompileCmdOpts{compFile, debug} = do
    content <- readFileText compFile
    let name = takeBaseName compFile
    datapackBS <- either failWithCompError pure =<< compileFileToDatapack (CompileOpts {
          fileName=toText compFile
        , name=toText name
        , debug
        }) content 
    writeFileLBS (name <> ".zip") datapackBS

failWithCompError :: CompilationError -> IO a
failWithCompError e = fail $ "CompilationError: " <> show e

data CobbleAction = Compile CompileCmdOpts deriving (Show, Eq)

data CompileCmdOpts = CompileCmdOpts {
      compFile :: FilePath
    , debug :: Bool
    } deriving (Show, Eq)

compileOpts :: Parser CompileCmdOpts
compileOpts = CompileCmdOpts
    <$> argument str (metavar "SOURCEFILE")
    <*> switch (long "debug" <> help "Debug mode keeps additional information at runtime")
