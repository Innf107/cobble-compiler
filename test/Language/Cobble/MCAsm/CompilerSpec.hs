module Language.Cobble.MCAsm.CompilerSpec where

import Language.Cobble.Prelude

import Language.Cobble.MCAsm.Types
import Language.Cobble.MCAsm.Compiler

import Test.Hspec
import Language.Cobble.Shared (Panic)

spec :: Spec
spec = do
    describe "compileInstr" do
        describe "MoveNumLit" do
            it "uses /scoreboard players set" do
                evalAsm (compileInstr (MoveNumLit (NumReg 0) 5))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set R0 REGS 5"
                    ]]
        describe "MoveNumReg" do
            it "uses /scoreboard players operation =" do
                evalAsm (compileInstr (MoveNumReg (NumReg 23) (NumReg 34)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R23 REGS = R34 REGS"
                    ]]
        describe "AddLit" do
            it "uses /scoreboard players add" do
                evalAsm (compileInstr (AddLit (NumReg 42) 3))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players add R42 REGS 3"
                    ]]
        describe "AddReg" do
            it "uses /scoreboard players operation +=" do
                evalAsm (compileInstr (AddReg (NumReg 1) (NumReg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R1 REGS += R2 REGS"
                    ]]
        describe "SubLit" do
            it "uses /scoreboard players remove" do
                evalAsm (compileInstr (SubLit (NumReg 1) 24))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players remove R1 REGS 24"
                    ]]
        describe "SubReg" do
            it "uses /scoreboard players operation -=" do
                evalAsm (compileInstr (SubReg (NumReg 1) (NumReg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R1 REGS -= R2 REGS"
                    ]]
        describe "MulLit" do
            it "uses CONST and /scoreboard players operation *=" do
                evalAsm (compileInstr (MulLit (NumReg 1) 2))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set CONST REGS 2"
                    ,   McFunction "scoreboard players operation R1 REGS *= CONST REGS"
                    ]]
        describe "MulReg" do
            it "uses /scoreboard players operation *=" do
                evalAsm (compileInstr (MulReg (NumReg 1) (NumReg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R1 REGS *= R2 REGS"
                    ]]
        describe "DivLit" do
            it "uses CONST and /scoreboard players operation /=" do
                evalAsm (compileInstr (DivLit (NumReg 1) 2))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set CONST REGS 2"
                    ,   McFunction "scoreboard players operation R1 REGS /= CONST REGS"
                    ]]
        describe "DivReg" do
            it "uses /scoreboard players operation /=" do
                evalAsm (compileInstr (DivReg (NumReg 1) (NumReg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R1 REGS /= R2 REGS"
                    ]]
        describe "Section" do
            it "creates a new InterModule" do
                evalAsm (compileInstr (Section "someSection" [AddLit (NumReg 1) 5, AddReg (NumReg 1) (NumReg 2)]))
                    `shouldBe` Right [InterModule "someSection" [
                        InterInstructions [McFunction "scoreboard players add R1 REGS 5"]
                    ,   InterInstructions [McFunction "scoreboard players operation R1 REGS += R2 REGS"]
                    ]]
        describe "Call" do
            it "uses /function with the correct namespace" do
                evalAsm (compileInstr (Call "someproc"))
                    `shouldBe` Right [InterInstructions [
                        McFunction "function test:someproc"
                    ]]
        describe "ExecEQ" do
            it "uses /execute if score =" do
                evalAsm (compileInstr (ExecEQ (NumReg 3) (NumReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS = R4 REGS run say Test"
                    ]]
        describe "ExecLT" do
            it "uses /execute if score <" do
                evalAsm (compileInstr (ExecLT (NumReg 3) (NumReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS < R4 REGS run say Test"
                    ]]
        describe "ExecGT" do
            it "uses /execute if score >" do
                evalAsm (compileInstr (ExecGT (NumReg 3) (NumReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS > R4 REGS run say Test"
                    ]]
        describe "ExecLE" do
            it "uses /execute if score <=" do
                evalAsm (compileInstr (ExecLE (NumReg 3) (NumReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS <= R4 REGS run say Test"
                    ]]
        describe "ExecGE" do
            it "uses /execute if score >=" do
                evalAsm (compileInstr (ExecGE (NumReg 3) (NumReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS >= R4 REGS run say Test"
                    ]]
        describe "ExecInRange" do
            it "uses /execute if score matches" do
                evalAsm (compileInstr (ExecInRange (NumReg 1) (RBounded -3 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R1 REGS matches -3..4 run say Test"
                    ]]
        describe "GetCommandResult" do
            it "uses execute store" do
                evalAsm (compileInstr (GetCommandResult (NumReg 1) (McFunction "gamerule randomTickSpeed")))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute store result score R1 REGS run gamerule randomTickSpeed"
                    ]]
        describe "GetBySelector" do
            it "sets the EPTR of the register and that of the entity to the same UID" do
                evalAsm (compileInstr (GetBySelector (EntityReg 1) "type=armor_stand"))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players add UID UID 1"
                    ,   McFunction "scoreboard players operation @e[type=armor_stand,limit=1] EPTR = UID UID"
                    ,   McFunction "scoreboard players operation E1 EPTR = UID UID"
                    ]]
        describe "RunCommandAsEntity" do
            it "uses execute as" do
                evalAsm (compileInstr (RunCommandAsEntity (EntityReg 1) (McFunction "say Test")))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e if score @s EPTR = E1 EPTR run say Test"
                    ]]
        describe "MoveEntity" do
            it "copies over the EPTR score with /scoreboard players operation =" do
                evalAsm (compileInstr (MoveEntity (EntityReg 1) (EntityReg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation E1 EPTR = E2 EPTR"
                    ]]
        describe "GetNumInArray" do
            it "reads the REGS score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's APTR" do
                evalAsm (compileInstr (GetNumInArray (NumReg 1) (ArrayReg 2) (NumReg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = A2 APTR if score @s IX = R4 REGS run scoreboard players operation R1 REGS = @s REGS"
                    ]]
        describe "GetEntityInArray" do
            it "reads the EPTR score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's APTR" do
                evalAsm (compileInstr (GetEntityInArray (EntityReg 1) (ArrayReg 2) (NumReg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = A2 APTR if score @s IX = R4 REGS run scoreboard players operation E1 EPTR = @s EPTR"
                    ]]
        describe "GetArrayInArray" do
            it "reads the APTR score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's APTR" do
                evalAsm (compileInstr (GetArrayInArray (ArrayReg 1) (ArrayReg 2) (NumReg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = A2 APTR if score @s IX = R4 REGS run scoreboard players operation A1 APTR = @s APTR"
                    ]]

        describe "SetScoreboard" do
            it "uses /scoreboard players operation =" do
                evalAsm (compileInstr (SetScoreboard (Objective "Obj") "Player" (NumReg 1)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation Player Obj = R1 REGS"
                    ]]
        describe "MoveArray" do
            it "copies the APTR score" do
                evalAsm (compileInstr (MoveArray (ArrayReg 1) (ArrayReg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation A1 APTR = A2 APTR"
                    ]]

        describe "TODO" do
            it "SetNumInArray"    $ pass @IO  -- Potentially has to spawn a new marker before setting (should include ARRAY tag)
            it "SetEntityInArray" $ pass @IO  --  ^
            it "SetArrayInArray"  $ pass @IO  --  ^
    describe "hoistModules" do
        it "Preserves order" do
            evalAsm (hoistModules (InterModule "SomeSection" [
                    InterInstructions [
                        McFunction "say 1"
                    ,   McFunction "say 2"
                    ]
                ,   InterModule "InnerSection" [
                        InterInstructions [
                            McFunction "say 3"
                        ,   McFunction "say 4"
                        ]
                    ]
                ,   InterInstructions [
                        McFunction "say 5"
                    ,   McFunction "say 6"
                    ]
                ]))
                `shouldBe`
                Right [   CompiledModule "SomeSection" (unlines ["say 1","say 2","say 5","say 6"])
                      ,   CompiledModule "SomeSection.InnerSection" (unlines ["say 3", "say 4"])
                      ]

data TestError = TestMcAsmError McAsmError
               | TestPanic Panic
               deriving (Show, Eq)

runAsm :: CompEnv
       -> CompState
       -> Sem '[Reader CompEnv, State CompState, Error Panic, Error McAsmError, Error TestError] a
       -> Either TestError a
runAsm env initialState = run . runError @TestError . mapError TestMcAsmError . mapError TestPanic . evalState initialState . runReader env

evalAsm :: Sem '[Reader CompEnv, State CompState, Error Panic, Error McAsmError, Error TestError] a
        -> Either TestError a
evalAsm = runAsm (CompEnv False "test") initialCompState
