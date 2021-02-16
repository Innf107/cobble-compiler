{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RankNTypes, ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE GADTs, LambdaCase, StandaloneDeriving, DataKinds, ConstraintKinds #-}
module Language.MCScript.MCAsm.Types where

import Language.MCScript.Prelude

import Polysemy
import qualified Polysemy.State as P
import qualified Polysemy.Reader as P

-- Can use an unlimited amount of Registers

type Name = Text

data Number

data Entity

data Array

data Register a where
    NumReg :: Int -> Register Number
    EntityReg :: Int -> Register Entity
    ArrayReg :: Int -> Register Array

deriving instance Show (Register a)
deriving instance Eq (Register a)

renderReg :: Register a -> Text
renderReg = \case
    NumReg    i -> "R" <> show i
    EntityReg i -> "E" <> show i
    ArrayReg  i -> "A" <> show i

data Module = Module {
      moduleName::Text
    , moduleInstructions::[Instruction]
    } deriving (Show, Eq)

data Instruction =
      MoveNumReg (Register Number) (Register Number)
    | MoveNumLit (Register Number) Int

    | AddReg (Register Number) (Register Number)
    | AddLit (Register Number) Int
    | SubLit (Register Number) Int
    | DivLit (Register Number) Int

    | PushReg (Register Number)
    | PushLit Int
    | PopNum (Register Number)

    | Section Name [Instruction]
    | CallEq (Register Number) (Register Number) Name
    | CallElse Name
    | Then

    | GetCommandResult (Register Number) Text

    | PushE (Register Entity)
    | PopE (Register Entity)
    | GetBySelector (Register Entity) Text
    
    | RunCommandAsEntity Text (Register Entity)

    | MakeArray (Register Array) (Register Number)
    --          ^final register  ^length
    | GetNumInArray (Register Number) (Register Array) (Register Number)
    --              ^final register   ^array           ^index
    | GetEntityInArray (Register Entity) (Register Array) (Register Number)
    --                 ^final register   ^array           ^index
    | SetNumInArray (Register Array) (Register Number) (Register Number)
    --              ^array           ^index            ^writing register
    | SetEntityInArray (Register Array) (Register Number) (Register Entity)
    --                 ^array           ^index            ^writing register
    deriving (Show, Eq)

data IntermediateResult = InterModule Name [IntermediateResult]
                        | InterInstructions Text
                        deriving (Show, Eq)

data CompiledModule = CompiledModule {
        compModName :: Name
      , compModInstructions :: Text
    }

data CompState = CompState {
    compUID::Int
} deriving (Show, Eq)

data CompEnv = CompEnv {
    debug::Bool
  , nameSpace::Text
}

type CompC r = Members '[P.Reader CompEnv, P.State CompState] r


{-
Example Program:

mov %1 5

mov %2 %1

add %1 1

push 111
push 222

pop %2
pop %3

.test
  add %1 100
  then
.test-else
  sub %1 100


calleq %1 %3 .test
callelse .test-else

getSelf E4

=>
~~~[main.mcfunction]~~~
# global init
summon minecraft:armor_stand 0 0 0 {Marker:1, Invisible:1, Tags:["STACK"]}

# register init
scoreboard objectives add REGS dummy

# debug init
scoreboard objectives setdisplay sidebar REGS

# mov %1 5
scoreboard players set R1 REGS 5
# mov %2 %1
scoreboard players operation R2 REGS = R1 REGS

# add %1 1
scoreboard players set CONST REGS 1
scoreboard players operation R1 REGS += CONST REGS

# push 111
execute positioned 0 0 0 at @e[tag=STACK, limit=1, sort=furthest] run summon armor_stand ~1 ~ ~ {Marker:1, Invisible:1, Tags:["STACK"]}
execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run scoreboard players set @s REGS 111

# push 222
execute positioned 0 0 0 at @e[tag=STACK, limit=1, sort=furthest] run summon armor_stand ~1 ~ ~ {Marker:1, Invisible:1, Tags:["STACK"]}
execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run scoreboard players set @s REGS 222


# pop %2
execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run scoreboard players operation R2 REGS = @s REGS
execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run kill @s

# pop %3
execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run scoreboard players operation R3 REGS = @s REGS
execute positioned 0 0 0 as @e[tag=STACK, limit=1, sort=furthest] run kill @s

# calleq %1 %3 .test
scoreboard players set ELSE REGS 1
execute if score R1 REGS = R3 REGS run function mcasm:main/test

# callelse %1 %3
scoreboard players set CONST REGS 1
execute if score ELSE REGS = CONST REGS run function mcasm:main/test2

~~~[main/test.mcfunction]~~~
# add %1 100
scoreboard players set CONST REGS 100
scoreboard players operation R1 REGS += CONST REGS

# then
scoreboard players set ELSE REGS 0

~~~[main/test-else.mcfunction]~~~
# sub %1 100
scoreboard players set CONST REGS 100
scoreboard players operation R1 REGS -= CONST REGS

-}
