{-#LANGUAGE NoImplicitPrelude, ScopedTypeVariables, OverloadedStrings#-}
module Main where

import Relude

import qualified Data.Text as T

import System.Directory

import qualified Data.List as L

import qualified Polysemy as P
import qualified Polysemy.State as P
import qualified Polysemy.Reader as P

import Language.MCScript.MCAsm.Compiler
import Language.MCScript.MCAsm.Types

prog :: Module
prog = Module "prog"
    [ MoveNumLit (NumReg 1) 5

    , MoveNumReg (NumReg 2) (NumReg 1)

    , AddLit (NumReg 1) 1

    , PushLit 111
    , PushLit 222

    , PopNum (NumReg 2)
    , PopNum (NumReg 3)

    , Section "test" [
          AddLit (NumReg 1) 100
        , Then
        ]
    , Section "test-else" [
            SubLit (NumReg 1) 100
        ]

    , CallEq (NumReg 1) (NumReg 3) "test"
    , CallElse "test-else"
    ]

queryTimePlayed :: Module
queryTimePlayed = Module "query-time-played" [
      GetCommandResult (NumReg 1) "time query gametime" -- Ticks

    , MoveNumReg (NumReg 2) (NumReg 1)                  -- Seconds
    , DivLit (NumReg 2) 20

    , MoveNumReg (NumReg 3) (NumReg 2)                  -- Minutes
    , DivLit (NumReg 3) 60

    , MoveNumReg (NumReg 4) (NumReg 3)                  -- Hours
    , DivLit (NumReg 4) 60
    ]

main :: IO ()
main = do
    path <- getHomeDirectory <&> (<>"/.minecraft/saves/New World (1)/datapacks/Test/data/mcasm/functions/")
    removeDirectoryRecursive path
    createDirectory path
    let modules = P.run $ P.runReader (CompEnv {debug=True,nameSpace="mcasm"}) $ P.evalState (CompState {compUID=0}) $
            compile [prog, queryTimePlayed]
    forM_ modules $ 
        \m -> writeFileWithParents (path ++ toString (compModName m) ++ ".mcfunction") (compModInstructions m)
    putStrLn "Done Compiling!"
    where
        writeFileWithParents :: FilePath -> Text -> IO ()
        writeFileWithParents fp content = createDirectoryIfMissing True (L.dropWhileEnd (/='/') fp)
            >> writeFileText fp content

