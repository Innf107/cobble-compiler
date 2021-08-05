module Language.Cobble.CPS.TopLevel.TypesSpec where


import Language.Cobble.Prelude
import Language.Cobble.CPS.TopLevel.Types

import Test.Hspec

spec :: Spec
spec = do
    describe "show instances" do
        it "prettyprints 'letf f k x y = add k x y in let h = halt in let x = 5 in let y = 4 in f h x y'" do
            show (LetF "f" "k" ["x", "y"] (App "add" ["k", "x", "y"]) 
                (C (Let "h" Halt (Let "x" (IntLit 5) (Let "y" (IntLit 4) (App "f" ["h", "x", "y"]))))))
                `shouldBe`
                intercalate "\n" [
                    "letf f k x y = (add k x y)"
                ,   "in"
                ,   "    let h = halt in" 
                ,   "    let x = 5 in" 
                ,   "    let y = 4 in" 
                ,   "    f h x y"
                ]