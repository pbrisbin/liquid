{-# LANGUAGE OverloadedStrings #-}
module Text.Liquid.ParseSpec (main, spec) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Text.Liquid.Parse" $ do
        context "Simple text" $ do
            it "parses exactly as-is" $ do
                let content = "Some{thing with \t 99% funny characters\n\n"

                content `shouldParseTo` [TString content]

        context "Output tags" $ do
            it "parses to a variable by the given name" $ do
                "foo {{bar}} baz" `shouldParseTo` [ TString "foo "
                                                  , TVar "bar"
                                                  , TString " baz"
                                                  ]

            it "can contain underscores and dots" $ do
                "{{user.phone_number}}" `shouldParseTo` [TVar "user.phone_number"]

            it "cannot contain numbers or spaces" $ do
                assertNoParse "{{ words and numb3rs }}"

        context "Logic tags" $ do
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

                it "parses an if statment with an else branch" $ do
                    "{% if foo %}consequent{% else %}alternative{% endif %}"
                        `shouldParseTo` [ TIf (TTruthy "foo")
                                            [ TString "consequent" ]
                                            (Just [ TString "alternative" ])
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
