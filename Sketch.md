# Sketches

## This contains (possibly unfinished) early sketches of potential new features

----------------------

### return
```
Int f(Int n){
    if (n <= 2)
        return 42
    doSomething()
    return 43
}
```
-->
```
[fib.mcfunction]
<set returned = 0>
execute if <returned == 0> if <n <= 2> run function fib-if
execute if <returned == 0> run <call doSomething>
execute if <returned == 0> run <set returned = 43>
execute if <returned == 0> run <set returnValue = fibRes>

<set returned = 0>
<#return return-value>

[fib-if.mcfunction]
<set returned = 1>
<set return-value = 42>
```

