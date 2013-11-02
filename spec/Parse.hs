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

        it "parses if statements with equality" $ do
            "oh {% if user.name == 'elvis presley' %}\nhey {{king}}.\n{% endif %}"
                `shouldParseTo` [ TString "oh "
                                , TIf (TEquals "user.name" "elvis presley")
                                    [ TString "\nhey "
                                    , TVar "king"
                                    , TString ".\n"
                                    ] Nothing
                                ]

        it "parses if statements with truthy tests" $ do
            "oh {% if user.name %}\nhey {{king}}.\n{% endif %}"
                `shouldParseTo` [ TString "oh "
                                , TIf (TTruthy "user.name")
                                    [ TString "\nhey "
                                    , TVar "king"
                                    , TString ".\n"
                                    ] Nothing
                                ]

    where
        str `shouldParseTo` ast = parseTemplate str `shouldBe` Right ast
