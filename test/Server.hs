module Server where

import Language.Cobble.Prelude
import Server.Framework

import System.Environment

test :: IO ()
test = do
    env <- getEnvironment
    testWithServer $
        [   testExprScore "test setup"
                "5"
                5

        ,   testExprScore "simple let"
                "let x = 5 in x"
                5
        ,   testExprScore "if (then)"
                "if _le 2 3 then 1 else 2"
                1
        ,   testExprScore "if (else)"
                "if _le 3 2 then 1 else 2"
                2
        ,   testSingleModScore "functions"
                (mconcat [
                    "const :: Int -> Int -> Int;"
                ,   "const x y = x;"
                ,   ""
                ,   "main :: Unit;"
                ,   "main = _setTestScoreboardUnsafe(const 6 2);"
                ])
                6
        ,   testSingleModScore "Linear recursion"
                (mconcat [
                    "gauss :: Int -> Int;"
                ,   "gauss n = if _le n 1 then 1 else _add n (gauss(_add n -1));"
                ,   ""
                ,   "main :: Unit;"
                ,   "main = _setTestScoreboardUnsafe(gauss(100));"
                ])
                5050
        ,   testSingleModScore "Branch recursion"
                (mconcat [
                    "fib :: Int -> Int;"
                ,   "fib n = if _le n 1 then 1 else _add (fib (_add n -1)) (fib (_add n -2));"
                ,   ""
                ,   "main :: Unit;"
                ,   "main = _setTestScoreboardUnsafe(fib(10));"
                ])
                89
        ,   testSingleModScore "Struct definition"
                (mconcat [
                    "struct S {"
                ,   "    x :: Int"
                ,   ",   y :: Bool"
                ,   "};"
                ,   ""
                ,   "f :: S -> Int;"
                ,   "f s = if s.y then s.x else 0;"
                ,   ""
                ,   "main :: Unit;"
                ,   "main = _setTestScoreboardUnsafe(f (S {x = 23, y = True}));"
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



