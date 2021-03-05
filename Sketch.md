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


### lambdas
```
[test.mcscript]
main(){
    f : int -> int = \x -> x * x;
    f(2);
    f(3);
}
```
-->
```
[test.mcscript]
main (){
    f = Function<int, int>(0, {})
--                         ^   ^ 
--             function index  closure

    call(f, 2)
    call(f, 3)
}


[functions.mcscript]
functions = [
      <mcfunction: call "_f_lambda_main"> -- 0

    ]

int _f_lambda_main(x: int){
    x * x
}

b call<a, b>(f: Function a b, x: a){
    functions[0]
}
```

