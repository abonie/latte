module Parsing.ParseSpec (spec) where
import Test.Hspec
import Parsing (parse)
import Parsing.ErrM


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "parse" $ do
        before (readFile "test/Parsing/bad001.lat") $ do
            it "fails on unclosed comment" $ \f -> do
                parse f `shouldSatisfy` foo


foo :: Err a -> Bool
foo (Ok _) = False
foo (Bad _) = True
