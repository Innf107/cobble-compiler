#!/usr/bin/env polaris
options {
    "-s" "--sync" as sync: "Run tests synchronously instead of in parallel"
    "-f" "--compiler-flag" (*) as compilerFlags: "An additional flag to pass to the cobble compiler (varargs)"
}

# There are three kinds of tests:
#  - If a test file just exists on its own, it is interpreted as a test that just checks successful compilation.
#    The majority of type checker tests are of this form. 
#  - If for a test file a.cb, there exists a file a.result, then the content of running a.cb should print the
#    exact contents of a.result to stdout
#  - If for a test file a.cb, there exists a file a.error, then compiling a.cb is supposed to fail with an
#    error message that exactly matches the content of a.error (modulo colors)

let List = require("list.pls")

let for = if sync then List.for else List.forConcurrent 

let files = lines(!find (scriptLocal(".")) "-name" "*.cb")

let removeFileExt(file) = file | sed "-E" "s/(.*)\..*/\1/" # No idea why there is no coreutils program for this...

# This should also REALLY be part of coreutils. (Also, this is *extremely* hacky)
let removeColors(string) = string | sed "-E" ('s/' ~ (!echo "-e" "\e") ~ '\[[^m]*m(' ~ (!echo "-e" "\2") ~ ')?//g')

let doesFileExist(file) = {
    let _ = !bash "-c" ("stat " ~ file ~ " 2> /dev/null")
    status() == 0
}

let join(sep, strings) = match strings {
    [] -> ""
    (s : strings) -> List.foldl(\(r, x) -> r ~ sep ~ x, s, strings)
}

let compile(file) = {
    let output = !bash "-c" 
        ("stack exec cobble -- compile '" 
        ~ file 
        ~ "' -o '" ~ (scriptLocal(".output/out.rkt")) ~ "' " 
        ~ join(" ", compilerFlags))
        ~ " 2>&1"
    [output, status() == 0]
}

!mkdir "-p" (scriptLocal(".output"))

!stack "build"
if status() != 0 then {
    exit(status())
} else {}

let failures = 0

for(files, \file -> {
    let baseFile = removeFileExt(file)

    if doesFileExist(baseFile ~ ".result") then {
        fail("result tests are not quite possible yet, sorry!")
    } else if doesFileExist(baseFile ~ ".error") then {
        # Test that compilation fails with the error message in (baseFile ~ ".error")
        let [output, success] = compile(file)
        if success then {
            !echo "-e" ("\e[31m[" ~ file ~ "](error): FAILED! (Compiled successfully despite errors)\e[0m")
            failures := failures + 1
        } else {
            let expected = replace("$FILE", file, !cat (baseFile ~ ".error"))
            let processed_output = output
            if removeColors(output) == expected then {
                !echo "-e" ("\e[32m[" ~ file ~ "](error): PASSED\e[0m")
            } else {
                !echo "-e" ("\e[31m[" ~ file ~ "](error): FAILED!\nExpected:\n" ~ expected ~ "\n" ~ "Actual:\n" ~ output ~ "\e[0m")
                failures := failures + 1
            }
        }
    } else {
        # In this case we only test for successful compilation
        let [output, success] = compile(file)
        if success then {
            # Using echo to work around the lack of escape codes in polaris
            !echo "-e" ("\e[32m[" ~ file ~ "](compile): PASSED\e[0m")
        } else {
            !echo "-e" ("\e[31m[" ~ file ~ "](compile): FAILED! Output:\n" ~ output ~ "\e[0m")
            failures := failures + 1
        }
    }
})

if failures == 0 then {
    !echo "-e" "\e[32mAll tests passed.\e[0m"
} else {
    !echo "-e" ("\e[31m" ~ failures ~ " TEST(S) FAILED!\e[0m")
    exit(failures)
}

