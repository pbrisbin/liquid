{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Liquid.Context
import Text.Liquid.Parse
import Text.Liquid.Render
import Text.Liquid

main :: IO ()
main = hspec $ do
    describe "renderPartWith" $ do
        it "renders simple strings" $ do
            renderPartWith undefined (TString "hello world")
                `shouldBe` Right "hello world"

        it "renders variable interpolation" $ do
            renderPartWith (fromList [("name", CVar "Pat")]) (TVar "name")
                `shouldBe` Right "Pat"

        it "renders nested variables" $ do
            let sub = fromList [("name", CVar "Pat"), ("age", CInt 28)]
            let ctx = fromList [("user", CSub sub)]

            renderPartWith ctx (TVar "user.name")
                `shouldBe` Right "Pat"

            renderPartWith ctx (TVar "user.age")
                `shouldBe` Right "28"
