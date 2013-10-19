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

        it "parses a for loop" $ do
            parseTemplate "{% for post in posts %}\nTitle: {{post.title}}\n{% endfor %}"
                    `shouldBe` Right [ TFor "post" "posts"
                                        [ TString "Title: "
                                        , TVar "post.title"
                                        ]
                                     ]


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

    describe "liquid" $ do
        it "can interpolate simple variables" $ do
            liquid [("name", CVar "Pat")] "Hi, my name is {{name}}."
                `shouldBe` Right "Hi, my name is Pat."
