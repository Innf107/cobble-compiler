module Language.Cobble.MCAsm.CompilerSpec where

import Language.Cobble.Prelude

import Language.Cobble.MCAsm.Types
import Language.Cobble.MCAsm.Compiler

import Test.Hspec
import Language.Cobble.Shared (Panic, Log, target117)

spec :: Spec
spec = do
    describe "compileInstr" do
        describe "MoveNumLit" do
            it "uses /scoreboard players set" do
                evalAsm (compileInstr (MoveNumLit (Reg 0) 5))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set R0 REGS 5"
                    ]]
        describe "MoveReg @'Number" do
            it "uses /scoreboard players operation =" do
                evalAsm (compileInstr (MoveReg (Reg 23) (Reg 34)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R23 REGS = R34 REGS"
                    ]]
        describe "AddLit" do
            it "uses /scoreboard players add" do
                evalAsm (compileInstr (AddLit (Reg 42) 3))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players add R42 REGS 3"
                    ]]
        describe "AddReg" do
            it "uses /scoreboard players operation +=" do
                evalAsm (compileInstr (AddReg (Reg 1) (Reg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R1 REGS += R2 REGS"
                    ]]
        describe "SubLit" do
            it "uses /scoreboard players remove" do
                evalAsm (compileInstr (SubLit (Reg 1) 24))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players remove R1 REGS 24"
                    ]]
        describe "SubReg" do
            it "uses /scoreboard players operation -=" do
                evalAsm (compileInstr (SubReg (NamedReg "Test") (Reg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation Test REGS -= R2 REGS"
                    ]]
        describe "MulLit" do
            it "uses CONST and /scoreboard players operation *=" do
                evalAsm (compileInstr (MulLit (Reg 1) 2))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set CONST REGS 2"
                    ,   McFunction "scoreboard players operation R1 REGS *= CONST REGS"
                    ]]
        describe "MulReg" do
            it "uses /scoreboard players operation *=" do
                evalAsm (compileInstr (MulReg (Reg 1) (Reg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R1 REGS *= R2 REGS"
                    ]]
        describe "DivLit" do
            it "uses CONST and /scoreboard players operation /=" do
                evalAsm (compileInstr (DivLit (Reg 1) 2))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players set CONST REGS 2"
                    ,   McFunction "scoreboard players operation R1 REGS /= CONST REGS"
                    ]]
        describe "DivReg" do
            it "uses /scoreboard players operation /=" do
                evalAsm (compileInstr (DivReg (Reg 1) (Reg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R1 REGS /= R2 REGS"
                    ]]
        describe "Section" do
            it "creates a new InterModule" do
                evalAsm (compileInstr (Section "someSection" [AddLit (Reg 1) 5, AddReg (Reg 1) (Reg 2)]))
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
                evalAsm (compileInstr (ExecEQ (Reg 3) (Reg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS = R4 REGS run say Test"
                    ]]
        describe "ExecLT" do
            it "uses /execute if score <" do
                evalAsm (compileInstr (ExecLT (Reg 3) (Reg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS < R4 REGS run say Test"
                    ]]
        describe "ExecGT" do
            it "uses /execute if score >" do
                evalAsm (compileInstr (ExecGT (Reg 3) (Reg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS > R4 REGS run say Test"
                    ]]
        describe "ExecLE" do
            it "uses /execute if score <=" do
                evalAsm (compileInstr (ExecLE (Reg 3) (Reg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS <= R4 REGS run say Test"
                    ]]
        describe "ExecGE" do
            it "uses /execute if score >=" do
                evalAsm (compileInstr (ExecGE (Reg 3) (Reg 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R3 REGS >= R4 REGS run say Test"
                    ]]
        describe "ExecInRange" do
            it "uses /execute if score matches" do
                evalAsm (compileInstr (ExecInRange (Reg 1) (RBounded -3 4) [McFunction "say Test"]))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute if score R1 REGS matches -3..4 run say Test"
                    ]]
        describe "GetCommandResult" do
            it "uses execute store" do
                evalAsm (compileInstr (GetCommandResult (Reg 1) (McFunction "gamerule randomTickSpeed")))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute store result score R1 REGS run gamerule randomTickSpeed"
                    ]]
        describe "GetBySelector" do
            it "sets the REGS of the register and that of the entity to the same UID" do
                evalAsm (compileInstr (GetBySelector (Reg 1) "type=armor_stand"))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players add UID UID 1"
                    ,   McFunction "scoreboard players operation @e[type=armor_stand,limit=1] REGS = UID UID"
                    ,   McFunction "scoreboard players operation R1 REGS = UID UID"
                    ]]
        describe "RunCommandAsEntity" do
            it "uses execute as" do
                evalAsm (compileInstr (RunCommandAsEntity (Reg 1) (McFunction "say Test")))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e if score @s REGS = R1 REGS run say Test"
                    ]]
        describe "MoveReg @'Entity" do
            it "copies over the REGS score with /scoreboard players operation =" do
                evalAsm (compileInstr (MoveReg (Reg 1) (Reg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R1 REGS = R2 REGS"
                    ]]
        describe "GetInArray @'Number" do
            it "reads the REGS score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's REGS" do
                evalAsm (compileInstr (GetInArray (Reg 1) (Reg 2) (Reg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = R2 REGS if score @s IX = R4 REGS run scoreboard players operation R1 REGS = @s REGS"
                    ]]
        describe "GetInArray @'Entity" do
            it "reads the REGS score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's REGS" do
                evalAsm (compileInstr (GetInArray (Reg 1) (Reg 2) (Reg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = R2 REGS if score @s IX = R4 REGS run scoreboard players operation R1 REGS = @s REGS"
                    ]]
        describe "GetInArray @'Array" do
            it "reads the REGS score of the ARRAY marker with the same IX score as the ixReg's IX and the same AELEM as the array's REGS" do
                evalAsm (compileInstr (GetInArray (Reg 1) (Reg 2) (Reg 4)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "execute as @e[tag=ARRAY] if score @s AELEM = R2 REGS if score @s IX = R4 REGS run scoreboard players operation R1 REGS = @s REGS"
                    ]]

        describe "SetScoreboard" do
            it "uses /scoreboard players operation =" do
                evalAsm (compileInstr (SetScoreboard (Objective "Obj") "Player" (Reg 1)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation Player Obj = R1 REGS"
                    ]]
        describe "Move @'Array" do
            it "copies the REGS score" do
                evalAsm (compileInstr (MoveReg (Reg 1) (Reg 2)))
                    `shouldBe` Right [InterInstructions [
                        McFunction "scoreboard players operation R1 REGS = R2 REGS"
                    ]]
        describe "SetInArrayOrNew" do
            it "creates a new member entity if it does not exist and sets its score" do
                evalAsm (compileInstr (SetInArrayOrNew (Reg 1) (Reg 2) (Reg 3)))
                    `shouldBe` Right [InterInstructions $ map McFunction [
                        "scoreboard players set ELSE REGS 1"
                    ,   "execute as @e[tag=ARRAY] if score @s AELEM = R1 REGS if score @s IX = R2 REGS run scoreboard players set ELSE REGS 0"
                    ,   "execute if score ELSE REGS matches 1..1 run summon minecraft:marker 0 0 0 {Tags:[ARRAY,TEMP]}"
                    ,   "scoreboard players operation @e[tag=TEMP] AELEM = R1 REGS"
                    ,   "scoreboard players operation @e[tag=TEMP] IX = R2 REGS"
                    ,   "tag @e[tag=TEMP] remove TEMP"
                    ,   "execute as @e[tag=ARRAY] if score @s AELEM = R1 REGS if score @s IX = R2 REGS run scoreboard players operation @s REGS = R3 REGS"
                    ]]
                    {-
                    scoreboard players set ELSE REGS 0
                    execute as @e[tag=ARRAY] if score @s AELEM = R1 REGS if score @s IX = R2 REGS run scoreboard players set ELSE REGS 1
                    execute if score ELSE REGS matches 1..1 run summon minecraft:area_effect_cloud 0 0 0 {Duration: 2147483647, Tags:[ARRAY,TEMP]}
                    -}

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
evalAsm = runAsm (CompEnv False "test" target117) initialCompState
