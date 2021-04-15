module Language.Cobble.MCAsm.CompilerSpec where

import Language.Cobble.Prelude

import Language.Cobble.MCAsm.Types
import Language.Cobble.MCAsm.Compiler

import Test.Hspec
import Language.Cobble.Shared (Panic, Log)

spec :: Spec
spec = do
    describe "compileInstr" do
        describe "MoveNumLit" do
            it "uses /scoreboard players set" do
                evalAsm (compileInstr (MoveNumLit (NumReg (VarReg 0)) 5))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set NV0 REGS 5"
                    ]]
        describe "MoveReg @'Number" do
            it "uses /scoreboard players operation =" do
                evalAsm (compileInstr (MoveReg (NumReg (VarReg 23)) (NumReg (VarReg 34))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation NV23 REGS = NV34 REGS"
                    ]]
        describe "AddLit" do
            it "uses /scoreboard players add" do
                evalAsm (compileInstr (AddLit (NumReg (VarReg 42)) 3))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players add NV42 REGS 3"
                    ]]
        describe "AddReg" do
            it "uses /scoreboard players operation +=" do
                evalAsm (compileInstr (AddReg (NumReg (VarReg 1)) (NumReg (VarReg 2))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation NV1 REGS += NV2 REGS"
                    ]]
        describe "SubLit" do
            it "uses /scoreboard players remove" do
                evalAsm (compileInstr (SubLit (NumReg (VarReg 1)) 24))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players remove NV1 REGS 24"
                    ]]
        describe "SubReg" do
            it "uses /scoreboard players operation -=" do
                evalAsm (compileInstr (SubReg (NumReg (NamedReg "Test")) (NumReg (VarReg 2))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation NTest REGS -= NV2 REGS"
                    ]]
        describe "MulLit" do
            it "uses CONST and /scoreboard players operation *=" do
                evalAsm (compileInstr (MulLit (NumReg (VarReg 1)) 2))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set NCONST REGS 2"
                    ,   McFunction "scoreboard players operation NV1 REGS *= NCONST REGS"
                    ]]
        describe "MulReg" do
            it "uses /scoreboard players operation *=" do
                evalAsm (compileInstr (MulReg (NumReg (VarReg 1)) (NumReg (VarReg 2))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation NV1 REGS *= NV2 REGS"
                    ]]
        describe "DivLit" do
            it "uses CONST and /scoreboard players operation /=" do
                evalAsm (compileInstr (DivLit (NumReg (VarReg 1)) 2))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set NCONST REGS 2"
                    ,   McFunction "scoreboard players operation NV1 REGS /= NCONST REGS"
                    ]]
        describe "DivReg" do
            it "uses /scoreboard players operation /=" do
                evalAsm (compileInstr (DivReg (NumReg (VarReg 1)) (NumReg (VarReg 2))))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation NV1 REGS /= NV2 REGS"
                    ]]
        describe "Section" do
            it "creates a new InterModule" do
                evalAsm (compileInstr (Section "someSection" [AddLit (NumReg (VarReg 1)) 5, AddReg (NumReg (VarReg 1)) (NumReg (VarReg 2))]))
                    `shouldBe` Right [InterModule "someSection" [
                        InterInstructions [McFunction "scoreboard players add NV1 REGS 5"]
                    ,   InterInstructions [McFunction "scoreboard players operation NV1 REGS += NV2 REGS"]
                    ]]
        describe "Call" do
            it "uses /function with the correct namespace" do
                evalAsm (compileInstr (Call "someproc"))
                    `shouldBe` Right [InterInstructions [
                        McFunction "function test:someproc"
                    ]]
        describe "ExecEQ" do
            it "uses /execute if score =" do
                evalAsm (compileInstr (ExecEQ (NumReg $ VarReg 3) (NumReg $ VarReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score NV3 REGS = NV4 REGS run say Test"
                    ]]
        describe "ExecLT" do
            it "uses /execute if score <" do
                evalAsm (compileInstr (ExecLT (NumReg $ VarReg 3) (NumReg $ VarReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score NV3 REGS < NV4 REGS run say Test"
                    ]]
        describe "ExecGT" do
            it "uses /execute if score >" do
                evalAsm (compileInstr (ExecGT (NumReg $ VarReg 3) (NumReg $ VarReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score NV3 REGS > NV4 REGS run say Test"
                    ]]
        describe "ExecLE" do
            it "uses /execute if score <=" do
                evalAsm (compileInstr (ExecLE (NumReg $ VarReg 3) (NumReg $ VarReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score NV3 REGS <= NV4 REGS run say Test"
                    ]]
        describe "ExecGE" do
            it "uses /execute if score >=" do
                evalAsm (compileInstr (ExecGE (NumReg $ VarReg 3) (NumReg $ VarReg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score NV3 REGS >= NV4 REGS run say Test"
                    ]]
        describe "ExecInRange" do
            it "uses /execute if score matches" do
                evalAsm (compileInstr (ExecInRange (NumReg $ VarReg 1) (RBounded -3 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score NV1 REGS matches -3..4 run say Test"
                    ]]
        describe "GetCommandResult" do
            it "uses execute store" do
                evalAsm (compileInstr (GetCommandResult (NumReg $ VarReg 1) (McFunction "gamerule randomTickSpeed")))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute store result score NV1 REGS run gamerule randomTickSpeed"
                    ]]
        describe "GetBySelector" do
            it "sets the EPTR of the register and that of the entity to the same UID" do
                evalAsm (compileInstr (GetBySelector (EntityReg $ VarReg 1) "type=armor_stand"))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players add UID UID 1"
                    ,   McFunction "scoreboard players operation @e[type=armor_stand,limit=1] EPTR = UID UID"
                    ,   McFunction "scoreboard players operation EV1 EPTR = UID UID"
                    ]]
        describe "RunCommandAsEntity" do
            it "uses execute as" do
                evalAsm (compileInstr (RunCommandAsEntity (EntityReg $ VarReg 1) (McFunction "say Test")))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e if score @s EPTR = EV1 EPTR run say Test"
                    ]]
        describe "MoveReg @'Entity" do
            it "copies over the EPTR score with /scoreboard players operation =" do
                evalAsm (compileInstr (MoveReg (EntityReg $ VarReg 1) (EntityReg $ VarReg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation EV1 EPTR = EV2 EPTR"
                    ]]
        describe "GetInArray @'Number" do
            it "reads the REGS score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's APTR" do
                evalAsm (compileInstr (GetInArray (NumReg $ VarReg 1) (ArrayReg $ VarReg 2) (NumReg $ VarReg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = AV2 APTR if score @s IX = NV4 REGS run scoreboard players operation NV1 REGS = @s REGS"
                    ]]
        describe "GetInArray @'Entity" do
            it "reads the EPTR score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's APTR" do
                evalAsm (compileInstr (GetInArray (EntityReg $ VarReg 1) (ArrayReg $ VarReg 2) (NumReg $ VarReg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = AV2 APTR if score @s IX = NV4 REGS run scoreboard players operation EV1 EPTR = @s EPTR"
                    ]]
        describe "GetInArray @'Array" do
            it "reads the APTR score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's APTR" do
                evalAsm (compileInstr (GetInArray (ArrayReg $ VarReg 1) (ArrayReg $ VarReg 2) (NumReg $ VarReg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = AV2 APTR if score @s IX = NV4 REGS run scoreboard players operation AV1 APTR = @s APTR"
                    ]]

        describe "SetScoreboard" do
            it "uses /scoreboard players operation =" do
                evalAsm (compileInstr (SetScoreboard (Objective "Obj") "Player" (NumReg $ VarReg 1)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation Player Obj = NV1 REGS"
                    ]]
        describe "Move @'Array" do
            it "copies the APTR score" do
                evalAsm (compileInstr (MoveReg (ArrayReg $ VarReg 1) (ArrayReg $ VarReg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation AV1 APTR = AV2 APTR"
                    ]]

        describe "TODO" do
            it "SetNumInArray"    $ pass @IO  -- Potentially has to spawn a new marker before setting (should include ARRAY tag)
            it "SetEntityInArray" $ pass @IO  --  ^
            it "SetArrayInArray"  $ pass @IO  --  ^
    describe "hoistModules" do
        it "Preserves statement order" do
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
                        ,   InterModule "InnerInnerSection" [
                            InterInstructions [
                                McFunction "say 5"
                            ]
                        ]
                    ]
                ,   InterInstructions [
                        McFunction "say 6"
                    ,   McFunction "say 7"
                    ]
                ]))
                `shouldBe`
                Right [   CompiledModule "SomeSection" (unlines ["say 1","say 2","say 6","say 7"])
                      ,   CompiledModule "InnerSection" (unlines ["say 3", "say 4"])
                      ,   CompiledModule "InnerInnerSection" "say 5\n"
                      ]
        it "is NOT related to the parent section" do
            evalAsm (hoistModules (InterModule "SomeSection" [
                    InterModule "InnerSection" [
                        InterModule "InnerInnerSection" []
                    ]
                ])) `shouldBe` Right [
                    CompiledModule "SomeSection" ""
                ,   CompiledModule "InnerSection" ""
                ,   CompiledModule "InnerInnerSection" ""
                ]

data TestError = TestMcAsmError McAsmError
               | TestPanic Panic
               deriving (Show, Eq)

runAsm :: CompEnv
       -> CompState
       -> Sem '[Reader CompEnv, State CompState, Error Panic, Error McAsmError, Error TestError, Output Log] a
       -> Either TestError a
runAsm env initialState = run . runOutputSem (const pass) . runError @TestError . mapError TestMcAsmError . mapError TestPanic . evalState initialState . runReader env

evalAsm :: Sem '[Reader CompEnv, State CompState, Error Panic, Error McAsmError, Error TestError, Output Log] a
        -> Either TestError a
evalAsm = runAsm (CompEnv False "test") initialCompState
