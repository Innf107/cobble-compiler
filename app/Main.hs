{-#LANGUAGE NoImplicitPrelude, ScopedTypeVariables, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Language.Cobble.Prelude hiding (argument)
import Language.Cobble

import Options.Applicative

main :: IO ()
main = runCobble =<< execParser (info (mainOpts <**> helper) mainInfo)
    where
        mainOpts = hsubparser (
              command "compile" (Compile <$> info compileOpts (progDesc "compile a file directly"))
            ) 
        mainInfo = idm

cobbleInfo :: InfoMod CobbleAction
cobbleInfo = mempty

runCobble :: CobbleAction -> IO ()
runCobble = \case
    Compile co -> runCompile co

runCompile :: CompileOpts -> IO ()
runCompile CompileOpts{compFile, debug} = do
    content <- readFileText compFile
    undefined -- compile 

data CobbleAction = Compile CompileOpts deriving (Show, Eq)

data CompileOpts = CompileOpts {
      compFile :: FilePath
    , debug :: Bool
    } deriving (Show, Eq)

compileOpts :: Parser CompileOpts
compileOpts = CompileOpts
    <$> argument str (metavar "SOURCEFILE")
    <*> switch (long "debug" <> help "Debug mode keeps additional information at runtime")
