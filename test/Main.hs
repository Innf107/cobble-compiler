module Main where

import Language.Cobble.Prelude hiding (lookupEnv)

import Language.Cobble.Types
import Language.Cobble.Typechecker

import qualified Data.Text as T

import Test.Hspec

import qualified Spec

import System.Environment

main :: IO ()
main = hspec Spec.spec
