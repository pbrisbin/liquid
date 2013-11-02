{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Liquid.Context
import Text.Liquid

main :: IO ()
main = hspec $ do
    describe "liquid" $ do
        it "can interpolate simple variables" $ do
            liquid [("name", CVar "Pat")] "Hi, my name is {{name}}."
                `shouldBe` Right "Hi, my name is Pat."
