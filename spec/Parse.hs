{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Liquid.Context
import Text.Liquid.Parse

-- HELPERS {{{
str `shouldParseTo` ast = parseTemplate str `shouldBe` Right ast

assertNoParse str =
    case parseTemplate str of
            Left _ -> True
            _      -> False
-- }}}

main :: IO ()
main = hspec $ do
    describe "Simple text" $ do
        it "parses exactly as-is" $ do
            let content = "Some{thing with \t 99% funny characters\n\n"

            content `shouldParseTo` [TString content]

    describe "Output tags" $ do
        it "parses to a variable by the given name" $ do
            "foo {{bar}} baz" `shouldParseTo` [ TString "foo "
                                              , TVar "bar"
                                              , TString " baz"
                                              ]

        it "can contain underscores and dots" $ do
            "{{user.phone_number}}" `shouldParseTo` [TVar "user.phone_number"]

        it "cannot contain numbers or spaces" $ do
            assertNoParse "{{ words and numb3rs }}"

    describe "Logic tags" $ do
        context "for loops" $ do
            it "parses to a for loop with the correct inner template" $ do
                "{% for elem in list %}foo {{elem.member}}{% endfor %}"
                    `shouldParseTo` [ TFor "elem" "list"
                                        [ TString "foo "
                                        , TVar "elem.member"
                                        ]
                                    ]

        context "if statements" $ do
            it "considers a single argument a truthy value test" $ do
                "{% if foo.bar %}foo {{foo.baz}}{% endif %}"
                    `shouldParseTo` [ TIf (TTruthy "foo.bar")
                                        [ TString "foo "
                                        , TVar "foo.baz"
                                        ] Nothing
                                    ]

            context "Operators" $ do
                it "parses equality between two objects" $ do
                    "{% if foo.bar == baz.bat %}content{% endif %}"
                        `shouldParseTo` [ TIf (TEquals "foo.bar" "baz.bat")
                                            [ TString "content" ] Nothing
                                        ]

                it "parses equality between an object and a string" $ do
                    "{% if foo.bar == 'baz, bat' %}content{% endif %}"
                        `shouldParseTo` [ TIf (TEquals "foo.bar" "baz, bat")
                                            [ TString "content" ] Nothing
                                        ]

                it "parses equality between an object and a number" $ do
                    "{% if foo.bar == 42 %}content{% endif %}"
                        `shouldParseTo` [ TIf (TEquals "foo.bar" "42")
                                            [ TString "content" ] Nothing
                                        ]
