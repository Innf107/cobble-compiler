Method definitions include the definitions of type class methods, as well as effect operations and Variant constructors.
Method definitions typically mention type variables from their constructs scope.

As an example, consider this definition of `Eq`.
```hs
class Eq a {
    eq :: a -> a -> Bool
}
```

When type checking and lowering `eq`, it's type has to be extended to 
```hs
eq :: ∀a μ1 μ2. Eq a => a -{μ1}> a -{μ2}> Bool
```
The interesting question is now: Should `a` and `Eq a` be part of the `∀` in the method definition? In other words: Should the **renamer** insert the `∀a` or should the **type checker** do this when collecting the type for `eq`?

In Cobble, these constraints are inserted by the **type checker**, so the renamer **does not** insert any types.
This is done to preserve the relationship between the `a` in the class definition and the `a` in the method.

In practice, this means that after renaming (visible with `--ddump-renamed`), the definition above should look like

```hs
class Eq a {
    eq :: ∀μ1 μ2. a -{μ1}> a -{μ2}> Bool
}
```

