{-# LANGUAGE ExistentialQuantification #-}
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
                  -- | An error representing a failed register cast.
                  -- Since casts are ALWAYS unsafe, this is an error in the
                  -- compiling code 
data McAsmError = RegisterCastError Text deriving (Show, Eq)

-- | Dangerous!
-- Can existentialize a register to allow unsafe casts (see castReg)
data SomeReg where
    SomeReg :: Register a -> SomeReg

deriving instance Show SomeReg

-- | Dangerous!
-- Reduces an existentialized `SomeReg` to a concrete `Register a`.
-- Throws an exception if the register types don't match!
class FromSomeReg a where
    fromSomeReg :: (Member (Error McAsmError) r) => SomeReg -> Sem r (Register a)

-- | Dangerous!
-- castReg . fromSomeReg == id iff the types are unchanged.
-- castReg . fromSomeReg :: Register a -> Register b is only safe if a ~ b, i.e.
-- if the type is known to not change and this function is only used because Haskell's
-- type system is getting in the way.
castReg :: (Member (Error McAsmError) r, FromSomeReg b) => Register a -> Sem r (Register b)
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

    | GetNumInArray (Register 'Number) (Register 'Array) (Register 'Number)
    --              ^final register   ^array           ^index
    | GetEntityInArray (Register 'Entity) (Register 'Array) (Register 'Number)
    --                 ^final register   ^array           ^index
    | GetArrayInArray (Register 'Array) (Register  'Array) (Register 'Number)
    --                ^final register   ^array           ^index
    | SetNumInArray (Register 'Array) (Register 'Number) (Register 'Number)
    --                ^array           ^index            ^writing register
    | SetEntityInArray (Register 'Array) (Register 'Number) (Register 'Entity)
    --                 ^array           ^index            ^writing register
    | SetArrayInArray (Register 'Array) (Register 'Number) (Register 'Array)
    --                 ^array           ^index            ^writing register
    | SetScoreboard Objective Text (Register 'Number)
    --                        ^player
    deriving (Show, Eq)

data IntermediateResult = InterModule Name [IntermediateResult]
                        | InterInstructions [McFunction]
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

newtype McFunction = McFunction { runMcFunction :: Text } deriving (Show, Eq)

newtype Objective = Objective { renderObjective :: Text } deriving (Show, Eq)

newtype Tag = Tag { renderTag :: Text } deriving (Show, Eq)


type CompC      r = Members '[Reader CompEnv, State CompState, Error McAsmError] r
type CompInnerC r = Members '[Reader CompEnv, State CompState, Error McAsmError, Writer [McFunction]] r


