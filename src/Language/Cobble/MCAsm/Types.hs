{-# LANGUAGE ExistentialQuantification #-}
module Language.Cobble.MCAsm.Types where

import Language.Cobble.Prelude

import Language.Cobble.Shared

import GHC.Show qualified as S

import Data.Data

import Data.Generics.Uniplate.Data

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
        _ -> throw $ RegisterCastError "Number"

instance FromSomeReg 'Entity where
    fromSomeReg = \case
        SomeReg (EntityReg i) -> pure $ EntityReg i
        _ -> throw $ RegisterCastError "Entity"

instance FromSomeReg 'Array where
    fromSomeReg = \case
        SomeReg (ArrayReg i) -> pure $ ArrayReg i
        _ -> throw $ RegisterCastError "Array"


data Register (t :: RegType) where
    NumReg :: RegId -> Register 'Number
    EntityReg :: RegId -> Register 'Entity
    ArrayReg :: RegId -> Register 'Array

data RegId = IdReg Int
           | NamedReg Text
           deriving (Show, Eq)

deriving instance Show (Register a)
deriving instance Eq (Register a)

renderReg :: Register a -> Text
renderReg = \case
    NumReg    i -> "N" <> renderRegId i
    EntityReg i -> "E" <> renderRegId i
    ArrayReg  i -> "A" <> renderRegId i

renderRegId :: RegId -> Text
renderRegId (IdReg i) = show i
renderRegId (NamedReg name) = name

data Module = Module {
      moduleName::Name
    , moduleInstructions::[Instruction]
    } deriving (Show)

data Instruction where
      MoveReg    :: (ObjForType t) => (Register t) -> (Register t) -> Instruction
      MoveNumLit :: (Register 'Number) -> Int -> Instruction

      AddReg :: (Register 'Number) -> (Register 'Number) -> Instruction
      AddLit :: (Register 'Number) -> Int -> Instruction
      SubReg :: (Register 'Number) -> (Register 'Number) -> Instruction
      SubLit :: (Register 'Number) -> Int -> Instruction
      MulReg :: (Register 'Number) -> (Register 'Number) -> Instruction
      MulLit :: (Register 'Number) -> Int -> Instruction
      DivReg :: (Register 'Number) -> (Register 'Number) -> Instruction
      DivLit :: (Register 'Number) -> Int -> Instruction

--    Signum (Register 'Number) -- Signum(x) = Min(Max(x, -1), 1)
      Min :: (Register 'Number) -> (Register 'Number) -> Instruction
      Max :: (Register 'Number) -> (Register 'Number) -> Instruction

      Section :: Name -> [Instruction] -> Instruction
      Call    :: Name -> Instruction

      ExecInRange :: (Register 'Number) -> Range -> [McFunction] -> Instruction
      ExecEQ :: (Register 'Number) -> (Register 'Number) -> [McFunction] -> Instruction
      ExecLT :: (Register 'Number) -> (Register 'Number) -> [McFunction] -> Instruction
      ExecGT :: (Register 'Number) -> (Register 'Number) -> [McFunction] -> Instruction
      ExecLE :: (Register 'Number) -> (Register 'Number) -> [McFunction] -> Instruction
      ExecGE :: (Register 'Number) -> (Register 'Number) -> [McFunction] -> Instruction

      GetCommandResult :: (Register 'Number) -> McFunction -> Instruction

      GetBySelector :: (Register 'Entity) -> Text -> Instruction
    
      RunCommandAsEntity :: (Register 'Entity) -> McFunction -> Instruction

      GetInArray :: (ObjForType t) => (Register t) -> (Register 'Array) -> (Register 'Number) -> Instruction
    --                      ^final register   ^array           ^index
      SetInArray :: (ObjForType t) => (Register 'Array) -> (Register 'Number) -> (Register t) -> Instruction
    --             ^array           ^index            ^writing register
      SetScoreboard :: Objective -> Text -> (Register 'Number) -> Instruction
    --                               ^player
deriving instance Show Instruction
--deriving instance Eq Instruction   somehow broken?


class ObjForType (t :: RegType) where objForType :: f t -> Objective
instance ObjForType 'Number where objForType _ = Objective "REGS"
instance ObjForType 'Entity where objForType _ = Objective "EPTR"
instance ObjForType 'Array  where objForType _ = Objective "APTR"


data IntermediateResult = InterModule Name [IntermediateResult]
                        | InterInstructions [McFunction]
                        deriving (Show, Eq)

data CompiledModule = CompiledModule {
        compModName :: Name
      , compModInstructions :: Text
    } deriving (Show, Eq)

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

newtype Objective = Objective { renderObjective :: Text } deriving (Show, Eq, Generic, Data, Typeable)

newtype Tag = Tag { renderTag :: Text } deriving (Show, Eq, Generic, Data, Typeable)


type CompC      r = Members '[Reader CompEnv, State CompState, Error McAsmError, Error Panic] r
type CompInnerC r = Members '[Reader CompEnv, State CompState, Error McAsmError, Error Panic, Writer [McFunction]] r


data Range = RInfEnd Int
           | RInfStart Int
           | RBounded Int Int
           deriving (Eq)

instance S.Show Range where
    show = \case
        RInfEnd i           -> show i <> ".."
        RInfStart i         ->  ".." <> show i
        RBounded minr maxr  ->  show minr <> ".." <> show maxr
