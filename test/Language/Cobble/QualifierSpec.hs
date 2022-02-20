module Language.Cobble.QualifierSpec where

import Language.Cobble.Prelude
import Language.Cobble.Typechecker as TC hiding (Type)
import Language.Cobble.Types

import Language.Cobble.Parser qualified as C
import Language.Cobble.Parser.Tokenizer qualified as C
import Language.Cobble.Qualifier qualified as C
import Language.Cobble.SemAnalysis qualified as C
import Language.Cobble.Prelude.Parser qualified as C (ParseError, parse)
import Language.Cobble.Util.Polysemy.Fresh qualified as C
import Language.Cobble.Util.Polysemy.Dump qualified as C
import Language.Cobble.Util.Polysemy.StackState qualified as C
import Language.Cobble qualified as C

import Language.Cobble.TestUtil

import Test.Hspec as S
import GHC.Base (errorWithoutStackTrace)

import Data.Map qualified as M

spec :: Spec
spec = do
    it "simple definitions are renamed properly" do
        runQualifier [
                "x :: Int;"
            ,   "x = 5;"
            ,   ""
            ,   "y :: Int;"
            ,   "y = let x = 6 in x;"
            ,   ""
            ,   "z :: Int;"
            ,   "z = x;"
            ] `shouldSatisfy` \case
                Right (Module _ _ [
                        Def _ _ (Decl _ x1 _ _) _
                    ,   Def _ _ (Decl _ _ _ (Let _ _ (Decl _ x2 _ _) (Var _ _ x2'))) _
                    ,   Def _ _ (Decl _ _ _ (Var _ _ x1')) _
                    ]) | x1 == x1' && x2 == x2' && x1 /= x2 -> True
                Right (Module _ _ sts) -> errorWithoutStackTrace (show sts)
                Left e -> errorWithoutStackTrace (show e)
    it "tyvars in constrained are qualified in the same scope as that constraints body" do
        runQualifier [
                "class Eq a {};"
            ,   "f :: Eq a => a;"
            ,   "f = f;"
            ] `shouldSatisfy` \case
                Right (Module _ _ [
                        _
                    ,   Def _ _ (Decl _ _ _ _) (TConstraint (MkConstraint _ (TVar a1)) (TVar a2))
                    ]) | a1 == a2 -> True
                Right (Module _ _ sts) -> errorWithoutStackTrace (show sts)
                Left e -> errorWithoutStackTrace (show e)

    it "typeclass parameters can be mentioned in method types" do
        runQualifier [
                "class Eq a {"
            ,   "    eq :: a -> a -> Bool;"
            ,   "};"
            ] `shouldSatisfy` \case
                Right (Module _ _ [
                        DefClass _ _ _ [a1] [
                            (_, TVar a2 :-> TVar a3 :-> _)
                        ]
                    ]) | a1 == a2 && a2 == a3 -> True
                Right (Module _ _ sts) -> errorWithoutStackTrace (show sts)
                Left e -> errorWithoutStackTrace (show e)
    it "ascriptions can mention in scope tyvars" do
        runQualifier [
                "variant Test a = MkTest;"
            ,   ""
            ,   "f :: a -> b -> Test a;"
            ,   "f x y = let z = MkTest :: Test a in z;"
            ] `shouldSatisfy` \case
                Right (Module _ _ [
                        _
                    ,   Def _ _ (Decl _ _ _ 
                            (Let _ _ (Decl _ _ _ (Ascription _ _ _ (TApp _ (TVar a1)))) _)) 
                            (TVar a2 :-> TVar b :-> TApp _ (TVar a3))
                    ]) | a1 == a2 && a2 == a3 -> True
                Right (Module _ _ sts) -> errorWithoutStackTrace (show sts)
                Left e -> errorWithoutStackTrace (show e)
    it "variables can be bound in case expressions" do
        runQualifier [
                "variant Identity a = MkIdentity a;"
            ,   ""
            ,   "getIdentity :: Identity a -> a;"
            ,   "getIdentity i = case i of {"
            ,   "    MkIdentity x -> x;"
            ,   "};"
            ] `shouldSatisfy` \case
                Right (Module _ _ [
                        _
                    ,   Def _ _ (Decl _ _ _ (Case _ _ _ 
                            [CaseBranch _ _ (ConstrP _ _ [VarP _ x1]) (Var _ _ x2)])) _
                    ]) | x1 == x2 -> True
                Right (Module _ _ sts) -> errorWithoutStackTrace (show sts)
                Left e -> errorWithoutStackTrace (show e)
    it "type variables keep their original names" do
        runQualifier [
                    "variant Test a = MkTest;"
                ,   ""
                ,   "f :: a -> b -> Test a;"
                ,   "f a b = let y = MkTest :: Test b in y;"
            ] `shouldSatisfy` \case
                Right (Module _ _ [
                        _
                    ,   Def _ _ _ (a1@(TVar (MkTVar (QName "a") _))
                                    :-> (TVar (MkTVar (QName "b") _))
                                    :-> (TApp _ (a2@(TVar (MkTVar (QName "a") _)))))
                    ]) | a1 == a2 -> True
                Right (Module _ _ sts) -> errorWithoutStackTrace (show sts)
                Left e -> errorWithoutStackTrace (show e)


header :: [Text]
header = [
        "module test;"
    ]

runQualifier :: [Text] -> Either C.QualificationError (Module SemAnalysis)
runQualifier content = run $ do
    toks <- either (\(e :: C.LexicalError) -> error $ "lex error: " <> show e) id <$> runError (C.tokenize "test.cb" (unlines (header <> content)))
    let parsed = either (\e -> error $ "parse error: " <> show e) id $ C.parse C.module_ "" toks
    
    let moduleSolved :: Module QualifyNames = let (Module () pmname psts) = parsed 
            in 
            Module (one (internalQName "prims", C.primModSig)) pmname (coercePass psts) 

    let qualScope = C.modSigToScope C.primModSig
    C.runFreshQNamesState $ runError @C.QualificationError $ C.evalStackStatePanic qualScope $ C.qualify moduleSolved

