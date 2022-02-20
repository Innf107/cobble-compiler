module Language.Cobble.TypecheckerSpec where

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

import Test.Hspec as S
import GHC.Base (errorWithoutStackTrace)

import Data.Map qualified as M

spec :: Spec
spec = do
    describe "typecheck" do
        it "really simple variable inference" do
            runTypecheck [
                    "x :: Int;"
                ,   "x = let y = 5 in let z = y in y;"
                ] `shouldSatisfy` \ast -> case [ty | Var (ty, _) _ (QName "y") :: Expr Codegen <- universeBi ast] of
                        [] -> False
                        xs -> all (== intT) xs

        it "no let generalization" do
            runTypecheck [
                    "x :: Int;"
                ,   "x = let f y = y in f 3;"
                ] `shouldSatisfy` \ast -> case [ty | VarType ty "f" <- universeBi ast ] of
                    [TCon (QName "Int") KStar :-> TCon (QName "Int") KStar] -> True
                    tys -> errorWithoutStackTrace $ toString $ ppTypes tys
        
        {-
        it "no let generalization [2]" do 
            runTypecheck [
                    "g :: Unit -> Int;"
                ,   "g x = 3;"

                -- workaround, since the test setup doesn't know about primops
                ,   "h :: Int -> Int -> Int;"
                ,   "h x y = x;"

                ,   "x :: Int;"
                ,   "x = let f y = y in h (f 5) (g (f ()));"
                ] `shouldSatisfy` \case
                    Left (DifferentConstructor _ (TCon (QName "Int") KStar) (TCon (QName "Unit") KStar)) -> True
                    e -> errorWithoutStackTrace $ show e 
        -}
        it "top level statements keep polymorphic type signature" do
            runTypecheck [
                    "f :: a -> a;"
                ,   "f x = x;"

                ,   "y :: Int;"
                ,   "y = f 5;"

                ,   "z :: Unit;"
                ,   "z = f ();"
                ] `shouldSatisfy` \ast -> case [ty | Def _ _ (Decl _ (QName "f") _ _) ty :: Statement Codegen <- universeBi ast] of
                    [TForall [MkTVar (QName "a") KStar] (TVar (MkTVar (QName "a") KStar) :-> TVar (MkTVar (QName "a") KStar))] -> True
                    tys -> errorWithoutStackTrace $ show ast -- toString $ ppTypes tys
        -- Not sure if this *really* works... seemed a bit too easy
        it "tyvars in top level bindings become skolems" do
            runTypecheck [
                    "f :: a -> a;"
                ,   "f x = 5;"
                ] `shouldSatisfy` \case
                    Left (SkolBinding _ (MkTVar (QName "a") KStar) (TCon (QName "Int") KStar)) -> True
                    e -> errorWithoutStackTrace $ show e

        it "different skolems don't unify" do
            runTypecheck [
                    "const :: a -> b -> a;"
                ,   "const x y = y;"
                ] `shouldSatisfy` \case
                    Left (SkolBinding _ (MkTVar (QName "b") KStar) (TSkol (MkTVar (QName "a") KStar))) -> True
                    e -> errorWithoutStackTrace $ show e

        it "polymorphic (forall) types are instantiated at usage site" do
            runTypecheck [
                    "f :: a -> a;"
                ,   "f x = x;"

                ,   "g :: Int;"
                ,   "g = f 5;"
                ] `shouldSatisfy` \ast -> case [ty | VarType ty "f" <- universeBi ast] of
                    [IntT :-> IntT] -> True
                    tys -> errorWithoutStackTrace $ toString $ ppTypes tys

        it "type variables and skolems unify" do
            runTypecheck [
                    "f :: a -> a;"
                ,   "f x = x;"

                ,   "const :: a -> b -> a;"
                ,   "const x y = f x;"
                ] `shouldSatisfy` \ast -> case [ty | App ty _ (VarType _ "f") _ <- universeBi ast] of
                    [TSkol (MkTVar (QName "a") KStar)] -> True
                    ts -> errorWithoutStackTrace $ toString $ ppTypes ts
        it "(recursive) variant constructors have the correct type" do
            runTypecheck [
                    "variant IntList = Nil"
                ,   "                | Cons Int IntList"
                ,   "                ;"
                ,   "f :: Int;"
                ,   "f = let x = Cons in 0;"
                ]  `shouldSatisfy` \ast -> case [ty | DeclType ty "x" <- universeBi ast] of
                    [IntT :-> (TCon (QName "IntList") KStar :-> TCon (QName "IntList") KStar)] -> True
                    [] -> errorWithoutStackTrace $ "No variables found. AST:\n" <> show ast 
                    ts -> errorWithoutStackTrace $ toString $ ppTypes ts
        it "polymorphic variant constructors have fully applied types" do
            runTypecheck [
                    "variant List a = Nil | Cons a (List a);"
                ,   "x :: Int;"
                ,   "x = let y = Nil in let z = y in 5;"
                ]   `shouldSatisfy` \ast -> case [ty | VarType ty "y" <- universeBi ast] of
                    [TApp (TCon (QName "List") (KStar `KFun` KStar)) _] -> True
                    [] -> errorWithoutStackTrace $ "No types found. AST:\n" <> show ast 
                    ts -> errorWithoutStackTrace $ toString $ ppTypes ts
        it "tyvars in polymorphic variant constructors are properly instantiated" do
            runTypecheck [
                    "variant Test a = MkTest;"
                ,   "x :: Test b;"
                ,   "x = MkTest;"
                ,   "y :: Test c;"
                ,   "y = MkTest;"
                ] `shouldSatisfy` isRight

        {-      
        it "typeclass methods include constraints" do
            runTypecheckWithState [
                    "class Eq a {"
                ,   "    eq :: a -> a -> Bool;"
                ,   "};"
                ,   ""
                ,   "x :: Bool;"
                ,   "x = eq 1 1;"
                ] `shouldSatisfy` \(TCState{_varTypes}, _) -> case [ty | (QName "eq", ty) <- M.toList _varTypes] of
                        [TForall [a1] (TConstraint (MkConstraint (QName "Eq") (TVar a2)) (TVar a3 :-> (TVar a4 :-> boolT)))]
                            | allEqual [a1, a2, a3, a4] -> True
                        [] -> errorWithoutStackTrace $ "Not found in state: " <> show _varTypes
                        ts -> errorWithoutStackTrace $ toString $ ppTypes ts
        -}
        it "typeclass methods are instantiated with constraints on variables" do
            runTypecheck [
                    "class Eq a {"
                ,   "    eq :: a -> a -> Bool;"
                ,   "};"
                ,   ""
                ,   "instance Eq Int {"
                ,   "    eq x y = if __le__ x y then __le__ y x else __false__ ();"
                ,   "};"
                ,   ""
                ,   "x :: Bool;"
                ,   "x = eq 1 1;"
                ] `shouldSatisfy` withAST' (show . first ppType) (\ast -> [(ty, cs) | Var (ty, cs) _ (QName "eq") :: Expr Codegen <- universeBi ast]) \case
                    (IntT :-> IntT :-> BoolT, [TWanted (MkConstraint (QName "Eq") IntT) _]) -> True
                    _ -> False
        it "application of typeclass methods lose constraints" do
            runTypecheck [
                    "class Eq a {"
                ,   "    eq :: a -> a -> Bool;"
                ,   "};"
                ,   ""
                ,   "instance Eq Int {"
                ,   "    eq x y = if __le__ x y then __le__ y x else __false__ ();"
                ,   "};"
                ,   ""
                ,   "x :: Bool;"
                ,   "x = eq 1 1;"
                ] `shouldSatisfy` withAST (\ast -> [ty | App ty _ (VarType _ "eq") _ :: Expr Codegen <- universeBi ast]) \case
                    BoolT -> True
                    _ -> False
        it "ascriptions narrow the type" do
            runTypecheck [
                    "variant Test a = MkTest;"
                ,   ""
                ,   "x :: Test a;"
                ,   "x = MkTest;"
                ,   ""
                ,   "y :: Test a;"
                ,   "y = x :: Test Int;"
                ] `shouldSatisfy` \case
                    Left (SkolBinding _ _ _) -> True
                    x -> errorWithoutStackTrace $ show x 
        it "ascriptions can mention in-scope type variables" do
            runTypecheck [
                    "variant Test a = MkTest;"
                ,   ""
                ,   "f :: a -> b -> Test a;"
                ,   "f a b = let y = MkTest :: Test b in y;"
                ] `shouldSatisfy` \case
                    Left (SkolBinding _ _ _) -> True
                    Right (Module _ _ sts) -> errorWithoutStackTrace (show sts)
                    Left e -> errorWithoutStackTrace $ show e

        it "higher-rank parameters" do
            runTypecheck [
                    "f :: a -> a;"
                ,   "f x = x;"
                ,   ""
                ,   "g :: (forall a. a -> a) -> Int -> Int;"
                ,   "g h x = let y = h x in y;"
                ,   ""
                ,   "main :: Unit;"
                ,   "main = __setTestScoreboardUnsafe__ (g f 5);"
                ] `shouldSatisfy` isRight
    describe "skolemize" do
        it "doesn't modify inner foralls" do
            let t1 = (TForall [MkTVar (internalQName "a") KStar] (TVar (MkTVar (internalQName "a") KStar)))
                    :->
                    intT
            runTCFresh (skolemize t1) `shouldSatisfy` \t2 ->
                if t2 == t1 then True 
                else errorWithoutStackTrace $ show t2



runUnify :: Sem '[Reader LexInfo, Output Log, Error TypeError] a -> Either TypeError a
runUnify = run . runError . ignoreOutput . runReader InternalLexInfo
    
runTCFresh :: Sem '[C.Fresh TVar TVar, C.Fresh Text QualifiedName, C.Fresh (Text, LexInfo) QualifiedName] a -> a
runTCFresh = run . C.runFreshQNamesState . C.freshWithInternal . C.runFreshM (\(MkTVar n k) -> C.freshVar (originalName n) <&> \n' -> MkTVar n' k)


withAST :: (Module Codegen -> [Type]) -> (Type -> Bool) -> Either TypeError (Module Codegen) -> Bool
withAST _ valid (Left err)      = errorWithoutStackTrace (show err) 
withAST match valid (Right ast) = case match ast of
    [t] | valid t  -> True
    [] -> errorWithoutStackTrace $ "Not found"
    ts -> errorWithoutStackTrace $ toString $ ppTypes ts

withAST' :: (a -> Text) -> (Module Codegen -> [a]) -> (a -> Bool) -> Either TypeError (Module Codegen) -> Bool
withAST' _ _ valid (Left err)      = errorWithoutStackTrace (show err)
withAST' pretty match valid (Right ast) = case match ast of
    [t] | valid t  -> True
    [] -> errorWithoutStackTrace $ "Not found"
    ts -> errorWithoutStackTrace $ toString $ unlines $ map pretty ts

ppTypes :: [Type] -> Text
ppTypes = unlines . map ppType

pattern VarType :: Type -> Text -> Expr Codegen
pattern VarType ty name <- Var (ty, _) _ (QName name)

pattern DeclType :: Type -> Text -> Decl Codegen
pattern DeclType ty name <- Decl (ty, _) (QName name) _ _

pattern QName :: Text -> QualifiedName
pattern QName name <- UnsafeQualifiedName name _ _

pattern IntT :: Type
pattern IntT <- ((==intT) -> True)
        where
            IntT = intT

pattern BoolT :: Type
pattern BoolT <- ((==boolT) -> True)
        where
            BoolT = boolT

runTypecheck :: [Text] -> Either TypeError (Module Codegen)
runTypecheck mod = run 
                 $ runError 
                 $ C.runFreshQNamesState
                 $ C.freshWithInternal 
                 $ C.runFreshM (\(MkTVar n k) -> C.freshVar (originalName n) <&> \n' -> MkTVar n' k)
                 $ C.dontDump @[TConstraint]
                 $ cobbleCode (unlines mod) >>= typecheck (TCEnv{_varTypes = M.map coercePass (exportedVars C.primModSig), _tcInstances = mempty})

cobbleCode :: (Member (C.Fresh (Text, LexInfo) QualifiedName) r) => Text -> Sem r (Module Typecheck)
cobbleCode content = do
    toks <- either (\(e :: C.LexicalError) -> error $ "lex error: " <> show e) id <$> runError (C.tokenize "test.cb" content)
    let parsed = either (\e -> error $ "parse error: " <> show e) id $ Module () "test" <$> C.parse C.statements "" toks
    
    let moduleSolved :: Module QualifyNames = let (Module () pmname psts) = parsed 
            in 
            Module (one (internalQName "prims", C.primModSig)) pmname (coercePass psts) 

    let qualScope = C.modSigToScope C.primModSig
    qualified <- fmap (either (\e -> error $ "qualification error:" <> show e) id) $ runError @C.QualificationError $ C.evalStackStatePanic qualScope $ C.qualify moduleSolved
    fmap (either (\e -> error $ "semantic error: " <> show e) id) $ runError @C.SemanticError $ C.runSemanticAnalysis qualified 


