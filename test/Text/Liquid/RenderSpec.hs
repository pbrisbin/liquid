{-# LANGUAGE OverloadedStrings #-}
module Text.Liquid.RenderSpec (main, spec) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Text.Liquid.Render" $ do
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
