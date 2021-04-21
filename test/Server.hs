module Server where

import Language.Cobble.Prelude
import Server.Framework

test :: IO ()
test = do
    testWithServer
        [   testSingleMod' "A simple setScoreboard works"
                "score TestScore TestPlayer = 5;"
                ["scoreboard objectives add TestScore dummy"]
                [TestQuery ["scoreboard players get TestPlayer TestScore"] $ ExpectLast $ expectScore "TestScore" "TestPlayer" 5]

        ,   testSingleMod' "Variables work"
                (mconcat [
                    "let x = 5;"
                ,   "score TestScore TestPlayer = x;"
                ])
                ["scoreboard objectivers add TestScore dummy"]
                [TestQuery ["scoreboard players get TestPlayer TestScore"] $ ExpectLast $ expectScore "TestScore" "TestPlayer" 5]
        ]
