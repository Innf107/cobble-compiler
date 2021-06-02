module Server where

import Language.Cobble.Prelude
import FullScale.FileTest
import Server.Framework

import System.Environment

test :: IO ()
test = do
    env <- getEnvironment
    runFileTestsRecursive "test/FullScale/"

futureAdditions :: [Test]
futureAdditions =
        [   testSingleModScore "Closure"
                (mconcat [
                    "Int f(x: Int){"
                ,   "    Int g(y: Int) => _add(x, y);"
                ,   "} => g(_add(x, 1));"
                ,   "score TestScore TestPlayer = f(4);"
                ])
                9
        ,   testSingleModScore "Simple Polymorphism"
                (mconcat [
                    "a id(x: a) => x;"
                ,   "score TestScore TestPlayer = id(42);"
                ])
                42
        ,   testSingleModScore "Forward references for functions"
                (mconcat [
                    "Int even (x: Int) => if _le(x, 0) then True else odd(_add(x, -1));"
                ,   "Int odd (x: Int) => if _le(x, 0) then False else even(_add(x, -1));"
                ,   "let x = if even(42) then 1 else -1;"
                ,   "score TestScore TestPlayer = x;"
                ])
                1
        ]



