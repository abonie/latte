module LLVM.LLVMSpec where
import Test.Hspec
import LLVM


spec :: Spec
spec = do
    describe "printModule" $ do
        it "handles single declaration" $ do
            moreorless (printModule test1) `shouldBe` moreorless test1'

        it "handles multiple declarations" $ do
            moreorless (printModule test2) `shouldBe` moreorless test2'

    where
        test1 = Module [FunDec Void (Ident "@printInt") [i32]]
        test1' = "declare void @printInt (i32)"
        test2 = Module [FunDec Void (Ident "@printInt") [i32],
                        FunDec i32 (Ident "@foo") [i1, i1],
                        FunDec i1 (Ident "@bar") []]
        test2' = "declare void @printInt (i32)\n\
                 \declare i32 @foo (i1, i1)\n\
                 \declare i1 @bar ()"


moreorless :: String -> String
moreorless x = unlines $ filter (not . (all (==' '))) $ map (reverse . foo . reverse . foo) $ lines x
    where
        foo (' ':t) = foo t
        foo x = x
