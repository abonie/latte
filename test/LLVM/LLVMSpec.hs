module LLVM.LLVMSpec where
import Test.Hspec
import LLVM

-- TODO
--

printModule = printTree


spec :: Spec
spec = do
    describe "printModule" $ do
        it "handles single declaration" $ do
            moreorless (printModule test1) `shouldBe` moreorless test1'

        it "handles multiple declarations" $ do
            moreorless (printModule test2) `shouldBe` moreorless test2'

    where
        test1 = Module [FunDec Void (Ident "@printInt") [Arg I32 (Ident "%x")]]
        test1' = "declare void @printInt (i32 %x)"
        test2 = Module [FunDec Void (Ident "@printInt") [Arg I32 (Ident "%x")],
                        FunDec I32 (Ident "@foo") [Arg I1 (Ident "%y"), Arg I1 (Ident "%z")],
                        FunDec I1 (Ident "@bar") []]
        test2' = "declare void @printInt (i32 %x)\n\
                 \declare i32 @foo (i1 %y, i1 %z)\n\
                 \declare i1 @bar ()"


moreorless :: String -> String
moreorless x = unlines $ filter (not . (all (==' '))) $ map (reverse . foo . reverse . foo) $ lines x
    where
        foo (' ':t) = foo t
        foo x = x
