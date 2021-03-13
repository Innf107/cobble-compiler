{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RankNTypes, ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE GADTs, LambdaCase, StandaloneDeriving, DataKinds, ConstraintKinds, FlexibleContexts #-}
module Language.Cobble.MCAsm.Types where

import Language.Cobble.Prelude

import Language.Cobble.Shared

-- Can use an unlimited amount of Registers

type Name = QualifiedName

-- | A DataKind representing the type of a register.
-- In Minecraft, `Number`s are represented as scoreboards,
-- `Entity`s are represented as Entity Pointers (see Documentation),
-- and `Array`s are represented as Entity Arrays (also see Documentation) 
data RegType = Number
             | Entity
             | Array

data McAsmError = RegisterCastError Text deriving (Show, Eq)

data SomeReg where
    SomeReg :: Register a -> SomeReg

deriving instance Show SomeReg

class FromSomeReg a where
    fromSomeReg :: (Member (Error McAsmError) r) => SomeReg -> Sem r (Register a)

-- | Dangerous!
-- castReg . fromSomeReg == id iff the types are unchanged.
-- castReg . fromSomeReg :: Register a -> Register b is only safe if a ~ b, i.e.
-- if the type is known to not change and this function is only used because Haskell's
-- type system is getting in the way.
castReg :: (Member (Error McAsmError) r, FromSomeReg a, FromSomeReg b) => Register a -> Sem r (Register b)
castReg = fromSomeReg . SomeReg

instance FromSomeReg 'Number where
    fromSomeReg = \case
        SomeReg (NumReg i) -> pure $ NumReg i
        SomeReg (CustomReg t) -> pure $ CustomReg t
        _ -> throw $ RegisterCastError "Number"

instance FromSomeReg 'Entity where
    fromSomeReg = \case
        SomeReg (EntityReg i) -> pure $ EntityReg i
        SomeReg (CustomReg t) -> pure $ CustomReg t
        _ -> throw $ RegisterCastError "Number"

instance FromSomeReg 'Array where
    fromSomeReg = \case
        SomeReg (ArrayReg i) -> pure $ ArrayReg i
        SomeReg (CustomReg t) -> pure $ CustomReg t
        _ -> throw $ RegisterCastError "Number"


data Register (t :: RegType) where
    NumReg :: Int -> Register 'Number
    EntityReg :: Int -> Register 'Entity
    ArrayReg :: Int -> Register 'Array
    CustomReg :: Text -> Register t

deriving instance Show (Register a)
deriving instance Eq (Register a)

renderReg :: Register a -> Text
renderReg = \case
    NumReg    i -> "R" <> show i
    EntityReg i -> "E" <> show i
    ArrayReg  i -> "A" <> show i
    CustomReg name -> name

data Module = Module {
      moduleName::Name
    , moduleInstructions::[Instruction]
    } deriving (Show, Eq)

data Instruction =
      MoveNumReg (Register 'Number) (Register 'Number)
    | MoveNumLit (Register 'Number) Int
    | MoveArray (Register 'Array) (Register 'Array)

    | AddReg (Register 'Number) (Register 'Number)
    | AddLit (Register 'Number) Int
    | SubLit (Register 'Number) Int
    | DivLit (Register 'Number) Int

    | Section Name [Instruction]
    | Call Name
    | CallEq (Register 'Number) (Register 'Number) Name
    | CallElse Name
    | Then

    | GetCommandResult (Register 'Number) Text

    | GetBySelector (Register 'Entity) Text
    
    | RunCommandAsEntity Text (Register 'Entity)

    | MakeArray (Register 'Array) (Register 'Number)
    --          ^final register  ^length
    | GetNumInArray (Register 'Number) (Register 'Array) (Register 'Number)
    --              ^final register   ^array           ^index
    | GetEntityInArray (Register 'Entity) (Register 'Array) (Register 'Number)
    --                 ^final register   ^array           ^index
    | GetArrayInArray (Register 'Array) (Register  'Array) (Register 'Number)
    --                ^final register   ^array           ^index
    | SetNumInArray (Register 'Array) (Register 'Number) (Register 'Number)
    --              ^array           ^index            ^writing register
    | SetEntityInArray (Register 'Array) (Register 'Number) (Register 'Entity)
    --                 ^array           ^index            ^writing register
    | SetArrayInArray (Register 'Array) (Register 'Number) (Register 'Array)
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

initialCompState :: CompState
initialCompState = CompState {
    compUID = 0
}

data CompEnv = CompEnv {
    debug::Bool
  , nameSpace::Text
}

type CompC r = Members '[Reader CompEnv, State CompState, Error McAsmError] r


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
