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
                evalAsm (compileInstr (MoveNumLit (NumReg (IdReg 0)) 5))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set N0 REGS 5"
                    ]]
        describe "MoveReg @'Number" do
            it "uses /scoreboard players operation =" do
                evalAsm (compileInstr (MoveReg (NumReg (IdReg 23)) (NumReg (IdReg 34))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation N23 REGS = N34 REGS"
                    ]]
        describe "AddLit" do
            it "uses /scoreboard players add" do
                evalAsm (compileInstr (AddLit (NumReg (IdReg 42)) 3))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players add N42 REGS 3"
                    ]]
        describe "AddReg" do
            it "uses /scoreboard players operation +=" do
                evalAsm (compileInstr (AddReg (NumReg (IdReg 1)) (NumReg (IdReg 2))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation N1 REGS += N2 REGS"
                    ]]
        describe "SubLit" do
            it "uses /scoreboard players remove" do
                evalAsm (compileInstr (SubLit (NumReg (IdReg 1)) 24))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players remove N1 REGS 24"
                    ]]
        describe "SubReg" do
            it "uses /scoreboard players operation -=" do
                evalAsm (compileInstr (SubReg (NumReg (NamedReg "Test")) (NumReg (IdReg 2))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation NTest REGS -= N2 REGS"
                    ]]
        describe "MulLit" do
            it "uses CONST and /scoreboard players operation *=" do
                evalAsm (compileInstr (MulLit (NumReg (IdReg 1)) 2))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set NCONST REGS 2"
                    ,   McFunction "scoreboard players operation N1 REGS *= NCONST REGS"
                    ]]
        describe "MulReg" do
            it "uses /scoreboard players operation *=" do
                evalAsm (compileInstr (MulReg (NumReg (IdReg 1)) (NumReg (IdReg 2))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation N1 REGS *= N2 REGS"
                    ]]
        describe "DivLit" do
            it "uses CONST and /scoreboard players operation /=" do
                evalAsm (compileInstr (DivLit (NumReg (IdReg 1)) 2))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set NCONST REGS 2"
                    ,   McFunction "scoreboard players operation N1 REGS /= NCONST REGS"
                    ]]
        describe "DivReg" do
            it "uses /scoreboard players operation /=" do
                evalAsm (compileInstr (DivReg (NumReg (IdReg 1)) (NumReg (IdReg 2))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation N1 REGS /= N2 REGS"
                    ]]
        describe "Section" do
            it "creates a new InterModule" do
                evalAsm (compileInstr (Section "someSection" [AddLit (NumReg (IdReg 1)) 5, AddReg (NumReg (IdReg 1)) (NumReg (IdReg 2))]))
                    `shouldBe` Right [InterModule "someSection" [
                        InterInstructions [McFunction "scoreboard players add N1 REGS 5"]
                    ,   InterInstructions [McFunction "scoreboard players operation N1 REGS += N2 REGS"]
                    ]]
        describe "Call" do
            it "uses /function with the correct namespace" do
                evalAsm (compileInstr (Call "someproc"))
                    `shouldBe` Right [InterInstructions [
                        McFunction "function test:someproc"
                    ]]
        describe "ExecEQ" do
            it "uses /execute if score =" do
                evalAsm (compileInstr (ExecEQ (NumReg $ IdReg 3) (NumReg $ IdReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score N3 REGS = N4 REGS run say Test"
                    ]]
        describe "ExecLT" do
            it "uses /execute if score <" do
                evalAsm (compileInstr (ExecLT (NumReg $ IdReg 3) (NumReg $ IdReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score N3 REGS < N4 REGS run say Test"
                    ]]
        describe "ExecGT" do
            it "uses /execute if score >" do
                evalAsm (compileInstr (ExecGT (NumReg $ IdReg 3) (NumReg $ IdReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score N3 REGS > N4 REGS run say Test"
                    ]]
        describe "ExecLE" do
            it "uses /execute if score <=" do
                evalAsm (compileInstr (ExecLE (NumReg $ IdReg 3) (NumReg $ IdReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score N3 REGS <= N4 REGS run say Test"
                    ]]
        describe "ExecGE" do
            it "uses /execute if score >=" do
                evalAsm (compileInstr (ExecGE (NumReg $ IdReg 3) (NumReg $ IdReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score N3 REGS >= N4 REGS run say Test"
                    ]]
        describe "ExecInRange" do
            it "uses /execute if score matches" do
                evalAsm (compileInstr (ExecInRange (NumReg $ IdReg 1) (RBounded -3 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score N1 REGS matches -3..4 run say Test"
                    ]]
        describe "GetCommandResult" do
            it "uses execute store" do
                evalAsm (compileInstr (GetCommandResult (NumReg $ IdReg 1) (McFunction "gamerule randomTickSpeed")))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute store result score N1 REGS run gamerule randomTickSpeed"
                    ]]
        describe "GetBySelector" do
            it "sets the EPTR of the register and that of the entity to the same UID" do
                evalAsm (compileInstr (GetBySelector (EntityReg $ IdReg 1) "type=armor_stand"))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players add UID UID 1"
                    ,   McFunction "scoreboard players operation @e[type=armor_stand,limit=1] EPTR = UID UID"
                    ,   McFunction "scoreboard players operation E1 EPTR = UID UID"
                    ]]
        describe "RunCommandAsEntity" do
            it "uses execute as" do
                evalAsm (compileInstr (RunCommandAsEntity (EntityReg $ IdReg 1) (McFunction "say Test")))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e if score @s EPTR = E1 EPTR run say Test"
                    ]]
        describe "MoveReg @'Entity" do
            it "copies over the EPTR score with /scoreboard players operation =" do
                evalAsm (compileInstr (MoveReg (EntityReg $ IdReg 1) (EntityReg $ IdReg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation E1 EPTR = E2 EPTR"
                    ]]
        describe "GetInArray @'Number" do
            it "reads the REGS score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's APTR" do
                evalAsm (compileInstr (GetInArray (NumReg $ IdReg 1) (ArrayReg $ IdReg 2) (NumReg $ IdReg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = A2 APTR if score @s IX = N4 REGS run scoreboard players operation N1 REGS = @s REGS"
                    ]]
        describe "GetInArray @'Entity" do
            it "reads the EPTR score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's APTR" do
                evalAsm (compileInstr (GetInArray (EntityReg $ IdReg 1) (ArrayReg $ IdReg 2) (NumReg $ IdReg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = A2 APTR if score @s IX = N4 REGS run scoreboard players operation E1 EPTR = @s EPTR"
                    ]]
        describe "GetInArray @'Array" do
            it "reads the APTR score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's APTR" do
                evalAsm (compileInstr (GetInArray (ArrayReg $ IdReg 1) (ArrayReg $ IdReg 2) (NumReg $ IdReg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = A2 APTR if score @s IX = N4 REGS run scoreboard players operation A1 APTR = @s APTR"
                    ]]

        describe "SetScoreboard" do
            it "uses /scoreboard players operation =" do
                evalAsm (compileInstr (SetScoreboard (Objective "Obj") "Player" (NumReg $ IdReg 1)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation Player Obj = N1 REGS"
                    ]]
        describe "Move @'Array" do
            it "copies the APTR score" do
                evalAsm (compileInstr (MoveReg (ArrayReg $ IdReg 1) (ArrayReg $ IdReg 2)))
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
