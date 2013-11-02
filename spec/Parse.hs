{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Liquid.Context
import Text.Liquid.Parse

main :: IO ()
main = hspec $ do
    describe "parseTemplate" $ do
        it "parses variable interpolations" $ do
            "Foo {{var}} bar" `shouldParseTo` [ TString "Foo "
                                              , TVar "var"
                                              , TString " bar"
                                              ]

        it "parses a for loop" $ do
            "{% for post in posts %}\nTitle: {{post.title}}\n{% endfor %}"
                    `shouldParseTo` [ TFor "post" "posts"
                                        [ TString "\nTitle: "
                                        , TVar "post.title"
                                        , TString "\n"
                                        ]
                                    ]

    where
        str `shouldParseTo` ast = parseTemplate str `shouldBe` Right ast
