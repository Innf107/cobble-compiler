{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Interactive where

import Language.Cobble.Prelude

import Language.Cobble.Interactive.Types
import Language.Cobble.Interactive.Effect qualified as E

import Language.Cobble as C

import Language.Cobble.Types

import Language.Cobble.Parser.Tokenizer qualified as T
import Language.Cobble.Parser qualified as P

import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Util.Polysemy.Dump

import Language.Cobble.Typechecker (TConstraint)

import Language.Cobble.Lua.Types
import Language.Cobble.Codegen.CobbleToLC qualified as C2LC
import Language.Cobble.LC.Types

data InteractiveState = InteractiveState {
        _modSigs :: Map QualifiedName ModSig
    ,   _imports :: [Statement QualifyNames]
    }

initialInteractiveState :: InteractiveState
initialInteractiveState = InteractiveState {
        _modSigs = mempty
    ,   _imports = []
    }

makeLenses ''InteractiveState


runInteractive :: Members '[Fresh (Text, LexInfo) QualifiedName] r => Sem (E.Interactive : r) a -> Sem r a
runInteractive = freshWithInternal . evalState initialInteractiveState . reinterpret2 \case
    E.Eval cmd -> eval cmd

eval :: Members '[State InteractiveState, Fresh Text QualifiedName] r => Text -> Sem r InteractiveOutput
eval content = wrapCompilationError do
    toks <- mapError LexError $ T.tokenize "<interactive>" content
    ast :: [Statement QualifyNames] <- fmap coercePass $ mapError ParseError $ fromEither $ P.parse P.statements "<interactive>" toks
    
    lastModSigs <- gets _modSigs

    modName <- newInteractiveModName
    
    (lc, sig) <- runCompile $ C.compileWithSig (Module (Ext lastModSigs) (renamed modName) ast)
    
    modify (modSigs %~ insert modName sig)
    modify (imports %~ (<> [makeImport undefined]))
    
    luaStmnts <- ignoreDumps $ compileFromLC (C2LC.collapseDefs lc)

    runInLua luaStmnts

runInLua :: [LuaStmnt] -> Sem r InteractiveOutput
runInLua = undefined

newInteractiveModName :: Members '[Fresh Text QualifiedName] r => Sem r QualifiedName
newInteractiveModName = (\qn -> unsafeQualifiedName (renamed qn) (renamed qn) (location qn)) <$> freshVar "interactive"

runCompile :: Sem (Fresh (Text, LexInfo) QualifiedName : Dump [TGiven] : Dump [TWanted] : Dump [TConstraint] : Output Log : r) a -> Sem r a
runCompile = ignoreOutput . dontDump . dontDump . dontDump . runFreshQNamesState

makeImport :: Text -> Statement QualifyNames
makeImport mod = (Import IgnoreExt InternalLexInfo mod)

wrapCompilationError :: Sem (Error CompilationError : r) InteractiveOutput -> Sem r InteractiveOutput
wrapCompilationError m = runError m <&> \case
    Right a -> a
    Left err -> InteractiveError err


