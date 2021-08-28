module Language.Cobble.Codegen.MCAsmToMCFunctionSpec where

import Language.Cobble.Codegen.MCAsmToMCFunction

import Language.Cobble.McFunction.Types

import Language.Cobble.Prelude
import Language.Cobble.TestUtil

import Test.Hspec

spec :: Spec
spec = do
    describe "createICallTree" do
        it "creates a correct tree for [\"f\", \"g\", \"h\", \"i\"]" do
            createICallTree ["f", "g", "h", "i"]
                `shouldBe` 
                    ([
                        ("icall/icall", [
                            Scoreboard (Players (Set (Player "%ICALLDONE") "REGS" 0))
                        ,   Function (Own "icall/node2")
                        ])
                    ,   ("icall/node2", [
                            Execute 
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (REQ 2))
                                $ ERun $ Scoreboard $ Players $ Set (Player "%ICALLDONE") "REGS" 1
                        ,   Execute
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (REQ 2))
                                $ ERun $ Function (Own "g")
                        ,   Execute
                                $ EIf (IScore (Player "%ICALLDONE") "REGS" $ IMatches (REQ 0))
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (RLE 2))
                                $ ERun $ Function (Own "icall/node1")
                        ,   Execute
                                $ EIf (IScore (Player "%ICALLDONE") "REGS" $ IMatches (REQ 0))
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (RGE 2))
                                $ ERun $ Function (Own "icall/node3")
                        ])
                    ,   ("icall/node1", [
                            Execute 
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (REQ 1))
                                $ ERun $ Scoreboard $ Players $ Set (Player "%ICALLDONE") "REGS" 1
                        ,   Execute
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (REQ 1))
                                $ ERun $ Function (Own "f")
                        ])
                    ,   ("icall/node3", [
                            Execute 
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (REQ 3))
                                $ ERun $ Scoreboard $ Players $ Set (Player "%ICALLDONE") "REGS" 1
                        ,   Execute
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (REQ 3))
                                $ ERun $ Function (Own "h")
                        ,   Execute
                                $ EIf (IScore (Player "%ICALLDONE") "REGS" $ IMatches (REQ 0))
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (RGE 3))
                                $ ERun $ Function (Own "icall/node4")
                        ])
                    ,   ("icall/node4", [
                            Execute 
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (REQ 4))
                                $ ERun $ Scoreboard $ Players $ Set (Player "%ICALLDONE") "REGS" 1
                        ,   Execute
                                $ EIf (IScore (Player "%ICALL") "REGS" $ IMatches (REQ 4))
                                $ ERun $ Function (Own "i")
                        ])
                    ], fromList [("f", 1), ("g", 2), ("h", 3), ("i", 4)])




