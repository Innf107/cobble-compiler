module Language.Cobble.MCAsm.Types where

import Language.Cobble.Prelude

import Language.Cobble.Shared

import GHC.Show qualified as S

import Data.Data

import Data.Generics.Uniplate.Data

-- Can use an unlimited amount of Registers

type Name = QualifiedName

data McAsmError deriving (Show, Eq)

data Register = NumReg    RegId
              | EntityReg RegId
              | ArrayReg  RegId
              deriving (Show, Eq)

data RegId = IdReg Int
           | NamedReg Text
           deriving (Show, Eq)

renderReg :: Register -> Text
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

data Instruction =
        MoveReg    Register Register
      | MoveNumLit Register Int

      | AddReg Register Register
      | AddLit Register Int
      | SubReg Register Register
      | SubLit Register Int
      | MulReg Register Register
      | MulLit Register Int
      | DivReg Register Register
      | DivLit Register Int

--    Signum (Register 'Number) -- Signum(x) = Min(Max(x, -1), 1)
      | Min Register Register
      | Max Register Register

      | Section Name [Instruction]
      | Call    Name

      | ExecInRange Register Range [McFunction]
      | ExecEQ Register Register [McFunction]
      | ExecLT Register Register [McFunction]
      | ExecGT Register Register [McFunction]
      | ExecLE Register Register [McFunction]
      | ExecGE Register Register [McFunction]

      | GetCommandResult Register McFunction

      | GetBySelector Register Text
    
      | RunCommandAsEntity Register McFunction

      | GetInArray Register Register Register
    --             ^final   ^array   ^index
      | SetInArrayOrNew Register Register Register
    --             ^array ^index   ^writing register
      | SetInArray Register Register Register
    --             ^array ^index   ^writing register
      | SetScoreboard Objective Text Register
    --                           ^player
      deriving (Show, Eq)


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


type CompC      r = Members '[Reader CompEnv, State CompState, Error McAsmError, Error Panic, Output Log] r
type CompInnerC r = Members '[Reader CompEnv, State CompState, Error McAsmError, Error Panic, Writer [McFunction], Output Log] r


data Range = RInfEnd Int
           | RInfStart Int
           | RBounded Int Int
           deriving (Eq)

instance S.Show Range where
    show = \case
        RInfEnd i           -> show i <> ".."
        RInfStart i         ->  ".." <> show i
        RBounded minr maxr  ->  show minr <> ".." <> show maxr


assertSameRegType :: (Member (Error Panic) r) => Register -> Register -> Sem r ()
assertSameRegType (NumReg _)    (NumReg _) = pass
assertSameRegType (EntityReg _) (EntityReg _) = pass
assertSameRegType (ArrayReg _)  (ArrayReg _) = pass
assertSameRegType r1 r2 = throw $ MismatchedRegTypes (show r1) (show r2)

assertRegNumber :: (Member (Error Panic) r) => Register -> Sem r ()
assertRegNumber (NumReg _) = pass
assertRegNumber r = throw $ MismatchedRegTypes (show r) "Number"

assertRegEntity :: (Member (Error Panic) r) => Register -> Sem r ()
assertRegEntity (EntityReg _) = pass
assertRegEntity r = throw $ MismatchedRegTypes (show r) "Entity"

assertRegArray :: (Member (Error Panic) r) => Register -> Sem r ()
assertRegArray (ArrayReg _) = pass
assertRegArray r = throw $ MismatchedRegTypes (show r) "Array"

