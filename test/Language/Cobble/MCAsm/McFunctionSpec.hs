module Language.Cobble.MCAsm.McFunctionSpec where

import Language.Cobble.Prelude

import Test.Hspec

import Language.Cobble.MCAsm.Types
import Language.Cobble.MCAsm.McFunction
import Language.Cobble.Shared

import Polysemy.Input

spec :: Spec
spec = do
    describe "rawCommand" do
        it "Only wraps its input in a single McFunction" do
            evalCompInner (rawCommand "say Test") `shouldBe` Right [McFunction "say Test"]
    describe "addScoreboardObjective" do
        it "gives the correct output" do
            evalCompInner (addScoreboardObjective (Objective "REGS"))
                `shouldBe`
                Right [McFunction "scoreboard objectives add REGS dummy"]
    describe "removeScoreboardObjective" do
        it "gives the correct output" do
            evalCompInner (removeScoreboardObjective (Objective "REGS"))
                `shouldBe`
                Right [McFunction "scoreboard objectives remove REGS"]
    describe "setScoreboardSidebar" do
        it "gives the correct output" do
            evalCompInner (setScoreboardSidebar (Objective "REGS"))
                `shouldBe`
                Right [McFunction "scoreboard objectives setdisplay sidebar REGS"]
    describe "scoreboardOperation" do
        it "gives the correct result for each operation" do
            evalCompInner (scoreboardOperation (Objective "REGS") "Player1" SAdd (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS += Player2 EPTR"]
            evalCompInner (scoreboardOperation (Objective "REGS") "Player1" SSub (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS -= Player2 EPTR"]
            evalCompInner (scoreboardOperation (Objective "REGS") "Player1" SMul (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS *= Player2 EPTR"]
            evalCompInner (scoreboardOperation (Objective "REGS") "Player1" SDiv (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS /= Player2 EPTR"]
            evalCompInner (scoreboardOperation (Objective "REGS") "Player1" SMod (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS %= Player2 EPTR"]
            evalCompInner (scoreboardOperation (Objective "REGS") "Player1" SAssign (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS = Player2 EPTR"]
            evalCompInner (scoreboardOperation (Objective "REGS") "Player1" SMin (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS < Player2 EPTR"]
            evalCompInner (scoreboardOperation (Objective "REGS") "Player1" SMax (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS > Player2 EPTR"]
            evalCompInner (scoreboardOperation (Objective "REGS") "Player1" SSwap (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS >< Player2 EPTR"]
    describe "setScoreboardForPlayer" do
        it "gives the correct result" do
            evalCompInner (setScoreboardForPlayer (Objective "REGS") "Player1" 5)
                `shouldBe`
                Right [McFunction "scoreboard players set Player1 REGS 5"]
    describe "addScoreboardForPlayer" do
        it "gives the correct result" do
            evalCompInner(addScoreboardForPlayer (Objective "REGS") "Player1" 5)
                 `shouldBe`
                 Right [McFunction "scoreboard players add Player1 REGS 5"]
    describe "subScoreboardForPlayer" do
        it "gives the correct result" do
            evalCompInner(subScoreboardForPlayer (Objective "REGS") "Player1" 5)
                 `shouldBe`
                 Right [McFunction "scoreboard players remove Player1 REGS 5"]
    describe "resetScoreboardForPlayer" do
        it "gives the correct result" do
            evalCompInner(resetScoreboardForPlayer (Objective "REGS") "Player1")
                 `shouldBe`
                 Right [McFunction "scoreboard players reset Player1 REGS"]
    describe "moveScoreboard" do
        it "gives the correct result" do
            evalCompInner (moveScoreboard (Objective "REGS") "Player1" (Objective "EPTR") "Player2")
                `shouldBe`
                Right [McFunction "scoreboard players operation Player1 REGS = Player2 EPTR"]
    describe "summomMarkerWithTags" do
        it "gives the correct result with arguments" do
            evalCompInner (summonMarkerWithTags [Tag "A", Tag "B"])
                `shouldBe`
                Right [McFunction "summon minecraft:area_effect_cloud 0 0 0 {Duration: 2147483647, Tags:[A,B]}"]
        it "gives the correct result without arguments" do
            evalCompInner (summonMarkerWithTags [])
                `shouldBe`
                Right [McFunction "summon minecraft:area_effect_cloud 0 0 0 {Duration: 2147483647, Tags:[]}"]
    describe "summonMarkerAtWithTags" do
        it "behaves like summonMarkerWithTags bit with a position" do
            evalCompInner (summonMarkerAtWithTags (Abs 1) (Abs 2) (Rel 3) [Tag "A", Tag "B"])
                `shouldBe`
                Right [McFunction "summon minecraft:area_effect_cloud 1 2 ~3 {Duration: 2147483647, Tags:[A,B]}"]
    describe "removeTag" do
        it "gives the correct result" do
            evalCompInner (removeTag (Tag "A") "@e[tag=A]")
                `shouldBe`
                Right [McFunction "tag @e[tag=A] remove A"]
    describe "execute" do
        describe "ERun" do
            it "wraps the full execute around every inner action" do
                evalCompInner (execute $ ERun (rawCommand "say A" >> rawCommand "say B"))
                    `shouldBe`
                    Right [McFunction "execute run say A", McFunction "execute run say B"]
                evalCompInner (execute $ EAs "@e[tag=A]" $ ERun (rawCommand "say A" >> rawCommand "say B"))
                    `shouldBe`
                    Right [McFunction "execute as @e[tag=A] run say A", McFunction "execute as @e[tag=A] run say B"]
        describe "TODO" do
            it "EAs" $ pass @IO 
            it "EAt" $ pass @IO
            it "EIn" $ pass @IO
            it "EAnchored" $ pass @IO
            it "EStoreRes" $ pass @IO
            it "EStoreSuccess" $ pass @IO
            it "EFacing" $ pass @IO
            it "EIf" $ pass @IO
        
data TestError = TestMcAsmError McAsmError
               | TestPanic Panic
               deriving (Show, Eq)
        
runCompInner :: CompEnv
             -> CompState
             -> Sem '[Reader CompEnv, State CompState, Writer [McFunction], Error McAsmError, Error Panic, Error TestError] a
             -> Either TestError ([McFunction], a)
runCompInner env initialState = run . runError @TestError . mapError TestPanic . mapError TestMcAsmError . runWriter . evalState initialState . runReader env

evalCompInner :: Sem '[Reader CompEnv, State CompState, Writer [McFunction], Error McAsmError, Error Panic, Error TestError] a
              -> Either TestError [McFunction]
evalCompInner = fmap fst . runCompInner (CompEnv True "Test") initialCompState
