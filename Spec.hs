{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Liquid.Context
import Text.Liquid.Parse
import Text.Liquid.Render
import Text.Liquid

main :: IO ()
main = hspec $ do
    describe "parseTemplate" $ do
        it "parses simple strings" $ do
            parseTemplate "String string"
                `shouldBe` Right [ TString "String string" ]

        it "parses variable interpolations" $ do
            parseTemplate "String {{var}} string"
                `shouldBe` Right [ TString "String "
                                 , TVar "var"
                                 , TString " string"
                                 ]

        it "parses variable interpolations at the front" $ do
            parseTemplate "{{var}} string"
                `shouldBe` Right [TVar "var", TString " string"]

        it "parses variable interpolations at the end" $ do
            parseTemplate "String {{var}}"
                `shouldBe` Right [TString "String ", TVar "var"]

    describe "renderPartWith" $ do
        it "renders simple strings" $ do
            renderPartWith undefined (TString "hello world")
                `shouldBe` Right "hello world"

        it "renders variable interpolation" $ do
            renderPartWith (fromList [("name", CVar "Pat")]) (TVar "name")
                `shouldBe` Right "Pat"

        it "handles invalid variables" $ do
            renderPartWith (fromList [("foo", CVar ""), ("bar", CVar "")]) (TVar "baz")
                `shouldBe` (Left $ "Unknown value in context"
                                 ++ "\n  missing value:   baz"
                                 ++ "\n  current context: [\"bar\",\"foo\"]")

    describe "liquid" $ do
        it "can interpolate simple variables" $ do
            liquid [("name", CVar "Pat")] "Hi, my name is {{name}}."
                `shouldBe` Right "Hi, my name is Pat."
