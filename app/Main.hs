module Main where

import Language.Cobble.Prelude hiding (argument)
import Language.Cobble
import Language.Cobble.Types
import Language.Cobble.Packager

import Options.Applicative
import Language.Cobble.Util.Polysemy.Time
import Language.Cobble.Util.Polysemy.Dump

import Language.Cobble.Lua.PrettyPrint

import Language.Cobble.Interactive qualified as I
import Language.Cobble.Interactive.Effect qualified as I

import Data.Text qualified as T

import HsLua qualified as Lua

import Polysemy.Embed

main :: IO ()
main = runCobble =<< execParser (info (mainOpts <**> helper) mainInfo)
    where
        mainOpts = hsubparser (
              command "compile" (Compile <$> info compileOpts (progDesc "Compile source files to a datapack"))
            ) <|> hsubparser (
                command "interactive" (Interactive <$> info interactiveOpts (progDesc "Run in an interactive REPL"))
            )
        mainInfo = idm

cobbleInfo :: InfoMod CobbleAction
cobbleInfo = mempty

runCobble :: CobbleAction -> IO ()
runCobble = \case
    Compile co -> runCompile co
    Interactive opts -> runInteractive opts

runCompile :: CompileCmdOpts -> IO ()
runCompile CompileCmdOpts{compFiles, debug, packageName, description, target, logLevel, ddumpTC, ddumpLC, ddumpCPS, ddumpReduced, ddumpTL, ddumpAsm} = do
    let opts = CompileOpts {
            name=packageName
        ,   debug
        ,   description
        ,   target
        ,   ddumpTC
        ,   ddumpLC
        ,   ddumpCPS
        ,   ddumpReduced
        ,   ddumpTL    
        ,   ddumpAsm
        }
    case target of
        MC117 -> do
            (logs, edatapackBS) <- runControllerC opts $ dumpToFilesWithConfig $ timeToIO $ compileToDataPack compFiles
            traverse_ (printLog logLevel) logs
            case edatapackBS of
                Left e -> failWithCompError e
                Right datapackBS -> writeFileLBS (toString packageName <> ".zip") datapackBS
        Lua -> do
            (logs, eluaFile) <- runControllerC opts $ dumpToFilesWithConfig $ compileToLuaFile compFiles
            traverse_ (printLog logLevel) logs
            case eluaFile of
                Left e -> failWithCompError e
                Right luaFile -> writeFileText (toString packageName <> ".lua") luaFile

runInteractive :: InteractiveCmdOpts -> IO ()
runInteractive InteractiveCmdOpts{ddumpLua} = do 
    printHeader
    Lua.run $ runM $ dumpWhenWithTo liftIO ddumpLua prettyLuaForRepl stderr $ I.runInteractive $ forever do
        embedLua $ putStr "λ> "
        embedLua $ hFlush stdout
        command <- embedLua $ getLine
        runInteractiveCommands command [
                ([":lua", ":l"],    \l -> I.evalLua l >>= embedLua . print)
            ,   ([":type", ":t"],   \l -> I.getType l >>= embedLua . print)
            ]
            (I.eval command >>= embedLua . print)
        where
            embedLua :: Members '[Embed I.Lua] r => IO a -> Sem r a
            embedLua = embed . liftIO

runInteractiveCommands :: Text -> [([Text], Text -> a)] -> a -> a
runInteractiveCommands input cmds def = go cmds
    where
        go [] = def
        go ((prefs, cmd) : cmds) = case asumMap (`T.stripPrefix` input) prefs of
            Just l -> cmd l
            Nothing -> go cmds

printHeader :: IO ()
printHeader = do
    putStrLn "Cobble Interactive"
    putStrLn ":? for help"

printLog :: LogLevel -> Log -> IO ()
printLog maxLevel (Log lvl o) = when (lvl <= maxLevel)
    $ putTextLn (logPrefix lvl <> o <> "\ESC[0m")
    where
        logPrefix = \case
            LogWarning -> "\ESC[38;2;255;128;0m\STX[WARNING] "
            LogInfo    -> "\ESC[38;2;0;225;225m\STX[INFO] "
            LogVerbose -> "\ESC[38;2;0;128;128m\STX[VERBOSE] "
            LogDebug -> "\ESC[38;2;0;225;0m\STX[DEBUG] "
            LogDebugVerbose -> "\ESC[38;2;0;128;0m\STX[DEBUG VERBOSE] "
            LogDebugVeryVerbose -> "\ESC[38;2;0;100;0m\STX[DEBUG VERY VERBOSE] "

-- TODO
failWithCompError :: CompilationError -> IO a
failWithCompError e = fail $ "CompilationError: " <> show e

data CobbleAction = Compile CompileCmdOpts 
                  | Interactive InteractiveCmdOpts
                  deriving (Show, Eq)

data CompileCmdOpts = CompileCmdOpts {
      compFiles :: [FilePath]
    , packageName :: Text
    , debug :: Bool
    , logLevel :: LogLevel
    , description :: Text
    , target :: Target
    , ddumpTC :: Bool
    , ddumpLC :: Bool
    , ddumpCPS :: Bool
    , ddumpReduced :: Bool
    , ddumpTL :: Bool
    , ddumpAsm :: Bool
    } deriving (Show, Eq)

data InteractiveCmdOpts = InteractiveCmdOpts {
    ddumpLua :: Bool
} deriving (Show, Eq)

compileOpts :: Parser CompileCmdOpts
compileOpts = CompileCmdOpts
    <$> some (argument str (metavar "SOURCEFILES"))
    <*> strOption (long "package-name" <> short 'p' <> metavar "NAME" <> help "The name of the package/datapack")
    <*> switch (long "debug" <> help "Debug mode keeps additional information at runtime")
    <*> option auto (long "log-level" <> metavar "LEVEL" <> value LogInfo <> help "Controls how much information is logged (LogWarning | LogInfo | LogVerbose | LogDebug | LogDebugVerbose)")
    <*> strOption (long "description" <> short 'd' <> metavar "DESCRIPTION" <> help "The datapack description for the datapack's pack.mcmeta")
    <*> option auto (long "target" <> metavar "TARGET" <> value MC117 <> help "Possible targets: mc-1.17, lua")
    <*> switch (long "ddump-tc" <> help "Write the typechecker constraints to a file")
    <*> switch (long "ddump-lc" <> help "Write the intermediate lambda calculus to a file")
    <*> switch (long "ddump-cps" <> help "Write the intermediate CPS to a file")
    <*> switch (long "ddump-reduced" <> help "Write the reduced intermediate CPS to a file")
    <*> switch (long "ddump-tl" <> help "Write the intermediate TopLevel CPS to a file")
    <*> switch (long "ddump-asm" <> help "Write the intermediate ASM to a file")

interactiveOpts :: Parser InteractiveCmdOpts
interactiveOpts = InteractiveCmdOpts
    <$> switch (long "ddump-lua" <> help "Write lua output to stderr")