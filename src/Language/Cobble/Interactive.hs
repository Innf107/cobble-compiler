{-#LANGUAGE TemplateHaskell#-}
module Language.Cobble.Interactive (
    module Language.Cobble.Interactive
,   module Language.Cobble.Interactive.Types
) where

import Language.Cobble.Prelude

import Language.Cobble.Interactive.Types
import Language.Cobble.Interactive.Effect qualified as E

import Language.Cobble as C hiding (Lua)

import Language.Cobble.Types

import Language.Cobble.Parser.Tokenizer qualified as T
import Language.Cobble.Parser qualified as P

import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Util.Polysemy.Dump

import Language.Cobble.Typechecker (TConstraint)

import Language.Cobble.Lua.Types
import Language.Cobble.Lua.PrettyPrint
import Language.Cobble.Codegen.CPSToLua (luaHeader)
import Language.Cobble.Codegen.CobbleToLC qualified as C2LC
import Language.Cobble.LC.Types

import Polysemy.Embed

import HsLua (LuaE)
import HsLua qualified as Lua

-- The type synonym in HsLua is fully applied, but we need it
-- as kind Type -> Type, so we have to redefine it here
type Lua = LuaE Lua.Exception 

data InteractiveState = InteractiveState {
        _modSigs :: Map QualifiedName ModSig
    ,   _imports :: [Statement QualifyNames]
    }

initialInteractiveState :: InteractiveState
initialInteractiveState = InteractiveState {
        _modSigs = one (internalQName "prims", primModSig)
    ,   _imports = []
    }

makeLenses ''InteractiveState



runInteractive :: Members '[Embed Lua, Dump [LuaStmnt]] r => Sem (E.Interactive : r) a -> Sem r a
runInteractive = runFreshQNamesState . evalState initialInteractiveState . addInit . reinterpret2 \case
    E.Eval cmd -> runEval cmd
    E.EvalLua cmd -> runInLuaRaw cmd
    E.GetType cmd -> runGetType cmd
    where
        addInit a = init *> a

init :: Members '[Embed Lua] r => Sem r ()
init = do
    embed $ Lua.openlibs
    runInLuaRaw luaHeader
    pure ()

runEval :: Members '[State InteractiveState, Fresh (Text, LexInfo) QualifiedName, Embed Lua, Dump [LuaStmnt]] r => Text -> Sem r InteractiveOutput
runEval content = wrapCompilationError $ freshWithInternal do
    toks <- mapError LexError $ T.tokenize "<interactive>" content
    ast <- fmap coercePass $ mapError ParseError $ fromEither $ P.parse (P.statements <* P.eof) "<interactive>" toks
    
    lastModSigs <- gets _modSigs
    lastImports <- gets _imports

    modName <- newInteractiveModName
    
    (lc, sig) <- runCompile $ C.compileWithSig (Module (Ext lastModSigs) (renamed modName) (lastImports <> ast))
    
    modify (modSigs %~ insert modName sig)
    modify (imports %~ (<> [makeImport (renamed modName)]))
    
    luaStmnts <- ignoreDumps $ compileFromLC (C2LC.collapseDefs lc)
    dump luaStmnts

    runInLua luaStmnts

runGetType :: Members '[State InteractiveState, Fresh (Text, LexInfo) QualifiedName, Embed Lua, Dump [LuaStmnt]] r 
           => Text 
           -> Sem r InteractiveOutput
runGetType exprText = do
    error ":type NYI"

runInLua :: Members '[Embed Lua] r => [LuaStmnt] -> Sem r InteractiveOutput
runInLua stmnts = runInLuaRaw (prettyLuaForRepl stmnts) 

runInLuaRaw :: Members '[Embed Lua] r => Text -> Sem r InteractiveOutput
runInLuaRaw luaCode = embed $ do
        status <- Lua.loadbuffer (encodeUtf8 luaCode) "interactive"
        case status of
            Lua.OK -> do 
                Lua.call 0 0
                pure Success
            _ -> pure $ LuaError status

newInteractiveModName :: Members '[Fresh Text QualifiedName] r => Sem r QualifiedName
newInteractiveModName = (\qn -> unsafeQualifiedName (renamed qn) (renamed qn) (location qn)) <$> freshVar "interactive"

runCompile :: Sem (Dump [TGiven] : Dump [TWanted] : Dump [TConstraint] : Output Log : r) a -> Sem r a
runCompile = ignoreOutput . dontDump . dontDump . dontDump

makeImport :: Text -> Statement QualifyNames
makeImport modName = (Import IgnoreExt InternalLexInfo modName)

wrapCompilationError :: Sem (Error CompilationError : r) InteractiveOutput -> Sem r InteractiveOutput
wrapCompilationError m = runError m <&> \case
    Right a -> a
    Left err -> InteractiveError err


