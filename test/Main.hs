{-#LANGUAGE NoImplicitPrelude, ScopedTypeVariables, OverloadedStrings, DataKinds#-}
{-# LANGUAGE MultiWayIf, LambdaCase #-}
module Main where

import Language.MCScript.Prelude

import Language.MCScript.Types
import Language.MCScript.Typechecker

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

{-
tcTests :: IO ()
tcTests = do
    testTypecheck "DeclCorrectPrim" [Decl "x" IntT (IntLit 5)] Successful
    testTypecheck "DeclIncorrectPrim" [Decl "x" IntT (BoolLit True)] (TCError (WrongDeclType "x" IntT BoolT))
    testTypecheck "DeclErrorInExprPrim" [Decl "x" IntT (FCall "doesNotExist" [])] (TCError (FunctionDoesNotExist "doesNotExist"))
    testTypecheck "DeclAndAssignCorrectPrim" [Decl "x" IntT (IntLit 3), Assign "x" (IntLit 4)] Successful
    testTypecheck "DeclAndAssignIncorrectPrim" [Decl "x" IntT (IntLit 3), Assign "x" (BoolLit False)]
        (TCError (WrongAssignType "x" IntT BoolT))
    testTypecheck "AssignErrorInExprPrim" [Decl "x" IntT (IntLit 0), Assign "x" (FCall "doesNotExist" [])]
        (TCError (FunctionDoesNotExist "doesNotExist"))
    testTypecheck "DefVoidCorrectNoArgs" [DefVoid "f" [] [Decl "x" IntT (IntLit 5), Assign "x" (IntLit 3)]] Successful
    testTypecheck "DefVoidIncorrectNoArgs" [DefVoid "f" [] [Decl "x" IntT (IntLit 5), Assign "x" (BoolLit True)]]
        (TCError (WrongAssignType "x" IntT BoolT))
    testTypecheck "DefVoidCorrectArgs" [DefVoid "f" [("x", IntT)] [Decl "y" IntT (Var "x")]] Successful
    testTypecheck "DefVoidIncorrectArgs" [DefVoid "f" [("x", IntT)] [Decl "y" BoolT (Var "x")]]
        (TCError (WrongDeclType "y" BoolT IntT))
    testTypecheck "DefFunCorrectNoArgs" [DefFun "f" [] [Decl "x" IntT (IntLit 5), Assign "x" (IntLit 3)] (IntLit 3) IntT]
        Successful
    testTypecheck "DefFunIncorrectNoArgs" [DefFun "f" [] [Decl "x" IntT (IntLit 5), Assign "x" (IntLit 3)] (Var "x") BoolT]
        (TCError (WrongReturnType "f" BoolT IntT))
    testTypecheck "DefFunErrorInBodyNoArgs" [DefFun "f" [] [Decl "x" IntT (IntLit 5), Assign "x" (BoolLit True)] (IntLit 3) IntT]
        (TCError (WrongAssignType "x" IntT BoolT))
    testTypecheck "DefFunCorrectArgs" [DefFun "f" [("x", IntT)] [Decl "y" IntT (Var "x")] (Var "y") IntT] Successful
    testTypecheck' (TCState mempty (fromList [("<", BoolT), ("+", IntT)]) (fromList [("<", [IntT, IntT]), ("+", [IntT, IntT])])) "WhileCorrect" 
        [Decl "i" IntT (IntLit 0), While (FCall "<" [Var "i", IntLit 10]) [Assign "i" (FCall "+" [Var "i", IntLit 1])]] Successful
    putTextLn "ToTest: CallVoid, CallFun, typeOf(Expr)"
-}
data TCResult = TCError TypeError
              | Successful
              deriving (Show, Eq)

testTypecheck :: Text -> [Statement 'Unaltered] -> TCResult -> IO ()
testTypecheck = testTypecheck' (TCState mempty mempty mempty)

testTypecheck' :: TCState -> Text -> [Statement 'Unaltered] -> TCResult -> IO ()
testTypecheck' tcstate name stmnts expected = do
    let res :: Either TypeError TCState = run $  runError $ execState tcstate $ traverse typecheck stmnts
    let successful = case (expected, res) of
            (TCError e, Left e') -> e == e'
            (Successful, Right _) -> True
            _ -> False
    if successful
    then putTextLn $ T.justifyLeft 30 ' ' name <> " \ESC[38;2;0;128;0mPASSED\ESC[0m\STX"
    else putTextLn $ T.justifyLeft 30 ' ' name <> " \ESC[38;2;255;0;0mFAILED!!!\n    Expected: " <> show expected
                                                          <> "\n    Received: " <> show res <> "\ESC[0m\STX"

