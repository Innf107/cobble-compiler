# Cobble

**Disclaimer**: This documentation is very incomplete and so far only covers very low level 
implementation details. Once the language is ready, a more detailed, higher level documentation will be available.


## Implementation Details
### Table of contents:
* [MCAsm Datatype Representation In Minecraft](#rep)
    - [Integers](#rep-integers)
    - [Entities](#rep-entities)
    - [Arrays](#rep-arrays)
* [Appendix](#appendix)
    - [Marker Entities](#marker-entity)
    - [UIDs](#uid)
    - [Pseudo-Players](#pseudo-players)

### <a id=rep></a> MCAsm Datatype Representation In Minecraft
#### <a id=rep-integers></a>**Integers**
Integers are implemented directly through the `REGS` scoreboard, 
which makes them the most flexible primitive datatype.
Integer Registers are represented as [Pseudo-Players](#pseudo-players) 
(Unless the registers are `CustomReg`s, the [Pseudo-Players](#pseudo-players) have the names `R1`, `R2`, ...), whereas
in [arrays](#rep-arrays), the integer values are stored as scores on the corresponding [marker entities](#marker-entity).

#### <a id=rep-entities></a>**Entities**
Entities are implemented through 'Entity Pointers'. For Registers these are 
[Pseudo-Players](#pseudo-players) (with the names `E1`, `E2`, ...) and values in [arrays](#rep-arrays) are 
stored as scores on the corresponding [marker entities](#marker-entity).

Entity Pointers (EP for short) are represented by a [UID](#uid) stored on the `EPTR` scoreboard. 
An Entity Correponds to an Entity Pointer, iff its `EPTR` score matches that of the EP.
If there is no such entity, the Entity Pointer is considered an `empty` EP. This can happen for example
if an entity referenced by an Entity Pointer dies or gets referenced by a different EP.

**Note**: Making one EP point to multiple entities is technically not impossible,
but will likely result in undefined behaviour (that's why EPs use [UIDs](#uid)).

**Note**: Despite the name, multiple Entity Pointers *CANNOT* point to the same entity.
If an entity is assigned to an EP, and is then later reassigned to a second EP, the first
one now becomes *empty*.

#### <a id=rep-arrays></a> **Arrays**
Because they use two different scoreboards, arrays are probably the most complicated primitive type.
Elements of arrays are represented as [marker entities](#marker-entity) with
two scores. The first scoreboard, `APTR` is very similar to the `EPTR` scorebaord for entites.
An Entity is an element of an Array, iff it's `APTR` score matches that of the array. Interestingly,
this allows for an O(1) implementation of a `contains` function on arrays. The element's index in the array
is determined by its `IX` value. As an example, the [marker entity](#marker-entity) for the element at index `5` of the array with `APTR=3` would 
have scores `APTR=3` and `IX=5`.
The *value* of an array (the array pointer if you will) is, as all values, either scored in a register 
(this time with names `A1`, `A2`, ...) or a [marker entity](#marker-entities) inside another entity. here the
[Pseudo-players](#pseudo-players) or [marker entities](#marker-entities) reference the array by its `APTR` value.

The [marker entites](#marker-entites) store values in a corresponding scoreboards. The exact details of this
are detailed in the corresponding sections, but note that trying to access an element of one type as an element of another type
is undefined behaviour.

**Note**: Technically, arrays can have multiple values at the same index or even multiple values of different types on the same [marker entity](#marker-entity). 
However, similar to [EPs](#rep-entites), this will likely result in undefined behaviour.

**Note**: Arrays do not have to be continuus. An array can for example have values at index `1` and `3`, but not `2`.
This might be dangerous though, since accessing an empty array index is generally undefined behaviour.

###<a id=appendix></a> Appendix

####<a id=marker-entity></a>**Marker Entities**
Marker Entites used to be represented by armor stands with the NBT tags `Invisible:1`
and `Marker:1`. Because of their massive performance improvements, this was changed to
area effect clouds with a Duration of `2^31-1` (The maximum value). Because the actual choice of marker
entities is irrelevant and the concrete choice might change again in the future, 
this document only uses the abstract term *marker entity*.

####<a id=uid></a>**UIDs**
Some implementeations need to pick unique numbers, which is where the `UID`
scoreboard comes in. Being only inhabited by a single [Pseudo-Player](#pseudo-players) also named `UID`, this
score only ever increases. This API is (currently) not exposed to MCAsm directly, although 
that might change in the future. When a UID is read from the `UID` [Pseudo-Player](#pseudo-players), 
the score is increased by 1. This makes sure, that all UID values are unique.

####<a id=pseudo-players></a>**Pseudo-Players**
This document mentions *Pseudo-Players* a lot. When assigning scoreboards in Minecraft,
there are no checks to make sure the player actually exists. In practice, this means that
a command like `scoreboard players set SomePlayer SomeObjective 42` will set the value of `SomeObjective`
for the "player" `SomePlayer` to `42`. Because `SomePlayer` is not actually a real player,
they are referred to as a *Pseudo-Player*.

**Note**: Technically, a datapack generated by MCAsm might break if a player with the
name `E1` (or `E2`, `E3`, ...) or `A1` (or `A2`, `A3`, ...) joins the game.
This will probably never happen, so you should not have to worry about it.

