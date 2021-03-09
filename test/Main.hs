{-#LANGUAGE NoImplicitPrelude, ScopedTypeVariables, OverloadedStrings, DataKinds#-}
{-# LANGUAGE MultiWayIf, LambdaCase #-}
module Main where

import Language.Cobble.Prelude

import Language.Cobble.Types
import Language.Cobble.Typechecker

import qualified Data.Text as T

import Test.Hspec

import qualified Spec

import System.Environment

import qualified Server

hasFlag :: Text -> Bool -> IO Bool
hasFlag flag def = do
    flagEnv <- toText . fromMaybe "" <$> lookupEnv (toString $ ("TEST" <> flag))
    case T.toLower flagEnv of
        "false" -> pure False
        "true" -> pure True
        _ -> fmap (T.toLower . toText) <$> lookupEnv "TESTALL" <&> \case
            Just "true" -> True
            _ -> def

main :: IO ()
main = do
    tc <- hasFlag "TC" True
    server <- hasFlag "SERVER" False
    when tc $ hspec Spec.spec
    when server $ Server.test

