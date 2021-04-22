module Server where

import Language.Cobble.Prelude
import Server.Framework

import System.Environment

test :: IO ()
test = do
    env <- getEnvironment
    testWithServer $
        [   testSingleModScore "A simple setScoreboard"
                "score TestScore TestPlayer = 5;"
                5

        ,   testSingleModScore "Variables"
                (mconcat [
                    "let x = 5;"
                ,   "score TestScore TestPlayer = x;"
                ])
                5
        ,   testSingleModScore "Assignment"
                (mconcat [
                    "let x = 5;"
                ,   "x = 3;"
                ,   "score TestScore TestPlayer = x;"
                ])
                3
        ,   testSingleModScore "If Statements without else"
                (mconcat [
                    "let x = 5;"
                ,   "if (_le(x, 6)){"
                ,   "    x = _add(x, 1);"
                ,   "};"
                ,   "if (_le(x, 3)){"
                ,   "    x = _add(x, -10);"
                ,   "};"
                ,   "score TestScore TestPlayer = x;"
                ])
                6
        ,   testSingleModScore "If Statements with else"
                (mconcat [
                    "let x = 5;"
                ,   "if (_le(x, 7)){"
                ,   "    x = _add(x, 1);"
                ,   "} else {"
                ,   "    x = _add(x, -10);"
                ,   "};"
                ,   "if (_le(x, -100)){"
                ,   "    x = _add(x, -20);"
                ,   "} else {"
                ,   "    x = _add(x, 2);"
                ,   "};"
                ,   "score TestScore TestPlayer = x;"
                ])
                8
        ,   testSingleModScore "Functions"
                (mconcat [
                    "Int double(x: Int) => _add(x, x);"
                ,   "score TestScore TestPlayer = double(3);"
                ])
                6
        ,   testSingleModScore "Linear recursion"
                (mconcat [
                    "Int gauss(n: Int) => if _le(n, 1) then 1 else _add(n, gauss(_add(n, -1)));"
                ,   "score TestScore TestPlayer = gauss(10);"
                ])
                55
        ,   testSingleModScore "Branch recursion"
                (mconcat [
                    "Int fib(n: Int) => if _le(n, 1) then 1 else _add(fib(_add(n, -1)), fib(_add(n, -2)));"
                ,   "score TestScore TestPlayer = fib(10);"
                ])
                89
        ,   testSingleModScore "Struct definition"
                (mconcat [
                    "struct S {"
                ,   "    x: Int"
                ,   ",   y: Bool"
                ,   "};"
                ,   "s = S { x=5, y=True };"
                ,   "score TestScore TestPlayer = s.x;"
                ])
                5
        ] <> whenFlag "FUTURE" False env futureAdditions

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



