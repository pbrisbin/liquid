{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Liquid.Context
import Text.Liquid.Parse
import Text.Liquid.Render

-- FIXTURES {{{
data Post = Post { postTitle :: Text }

instance ToJSON Post where
    toJSON (Post title) = object ["title" .= title]

posts = [ Post "Post one"
        , Post "Post two"
        , Post "Post three"
        ]

data User = User
    { userName  :: Text
    , userAge   :: Int
    , userPosts :: [Post]
    }

user = User "Pat" 28 posts

instance ToJSON User where
    toJSON (User name age posts) =
        object ["user" .= object [ "name"  .= name
                                 , "age"   .= age
                                 , "posts" .= map toJSON posts
                                 ]]
-- }}}

-- HELPERS {{{
str `shouldParseTo` ast = parseTemplate str `shouldBe` Right ast

(ctx, ast) `shouldRender` txt = renderWith ctx ast `shouldBe` Right txt

assertNoParse str =
    case parseTemplate str of
            Left _ -> True
            _      -> False

assertNoRender ctx ast =
    case renderWith ctx ast of
        Left _  -> True
        _       -> False
-- }}}

main :: IO ()
main = hspec $ do
    describe "Parsing" $ do
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

    describe "Rendering" $ do
        context "Simple text" $ do
            it "renders exactly as-is" $ do
                (undefined, [(TString "hello world")]) `shouldRender` "hello world"

        context "Output tags" $ do
            it "renders the variable out of the given context" $ do
                let value = "Foo" :: Text

                (object ["name" .= value], [(TVar "name")]) `shouldRender` value

            it "renders nested variables" $ do
                let pat = user { userName = "Pat", userAge = 28 }

                (toJSON pat, [(TVar "user.name")]) `shouldRender` "Pat"

                (toJSON pat, [(TVar "user.age")]) `shouldRender` "28"

            it "fails on missing values" $ do
                assertNoRender (toJSON user) [(TVar "i.dont.exist")]

        context "Logic tags" $ do
            context "for loops" $ do
                it "renders with elements in the given collection" $ do
                    let pat = user { userPosts = [ Post "Post one"
                                                 , Post "Post two"
                                                 ] }

                    (toJSON pat, [TFor "post" "user.posts" [TVar "post.title"]])
                        `shouldRender` "Post onePost two"

                it "maintains access to the outer context" $ do
                    let pat = user { userName  = "Pat"
                                   , userPosts = [Post "", Post ""] }

                    (toJSON pat, [TFor "post" "user.posts" [TVar "user.name"]])
                        `shouldRender` "PatPat"

            context "if statements" $ do
                it "renders when a truthy conditions is true" $ do
                    (object ["var" .= True], [TIf (TTruthy "var") [TString "content"] Nothing])
                        `shouldRender` "content"

                it "renders nothing if truthy condition is false with no alternative" $ do
                    (object ["var" .= False], [TIf (TTruthy "var") [TString "content"] Nothing])
                        `shouldRender` ""

                it "renders the alternative truthy condition is false" $ do
                    (object ["var" .= False], [TIf (TTruthy "var") [] (Just [TString "content"])])
                        `shouldRender` "content"

                context "Operators" $ do
                    let apple = "apple" :: Text

                    it "renders when an equality predicate is true" $ do
                        (object ["var" .= apple], [TIf (TEquals "var" "apple") [TString "content"] Nothing])
                            `shouldRender` "content"

                    it "does not render when an equality predicate is false" $ do
                        (object ["var" .= apple], [TIf (TEquals "var" "orange") [TString "content"] Nothing])
                            `shouldRender` ""

                    it "supports equality checks between two objects" $ do
                        let ctx = object ["var" .= object ["one" .= apple, "two" .= apple]]

                        (ctx, [TIf (TEquals "var.one" "var.two") [TString "content"] Nothing])
                            `shouldRender` "content"
