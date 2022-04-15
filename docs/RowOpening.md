# Row Opening
Cobble uses Row Polymorphism to type effectful functions.
Unlinke in most forms of row polymorphism, closed records are never actually useful in Cobble's type system.

A function with a closed function type `A -{ϵ}> B` can only ever be used in a function with exactly the same effects. The only real reason to have these kinds of types is to allow functions that take *pure* functions, or functions that are limited to certain effects as arguments. The issue with this is, that with these closed function types, the outer function cannot actually apply the pure function, unless it is also pure.

This is obviously not great and Cobble features a much better solution anyway. Instead of `(A -{}> B) -> C`, a much nicer solution is the rank 2 type `(forall e. A -{e}> B) -> C`, which still only allows the effects mentioned in `A -{e}> B`, but which allows the function body to actually apply the pure function without spreading purity.

For this reason, cobble automatically opens every seemingly closed function type before typechecking, by inserting a fresh type variable. A function type `A -{Eff1, Eff2}> B` therefore becomes `A -{Eff1, Eff2 | μ}> B`, where μ is a fresh variable. This means that programmers never have to worry about row polymorphism if they don't need it.

