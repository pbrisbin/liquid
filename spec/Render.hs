{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Liquid.Context
import Text.Liquid.Parse
import Text.Liquid.Render
import Text.Liquid

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

main :: IO ()
main = hspec $ do
    describe "Simple text" $ do
        it "renders exactly as-is" $ do
            (undefined, [(TString "hello world")]) `shouldRender` "hello world"

    describe "Output tags" $ do
        it "renders the variable out of the given context" $ do
            let value = "Foo" :: Text

            (object ["name" .= value], [(TVar "name")]) `shouldRender` value

        it "renders nested variables" $ do
            let pat = user { userName = "Pat", userAge = 28 }

            (toJSON pat, [(TVar "user.name")]) `shouldRender` "Pat"

            (toJSON pat, [(TVar "user.age")]) `shouldRender` "28"

        it "fails on missing values" $ do
            assertNoRender (toJSON user) [(TVar "i.dont.exist")]

    describe "Logic tags" $ do
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
                pending

            it "does not render when a truthy condition is false" $ do
                pending

            it "renders when a predicate is true" $ do
                pending

            it "does not render when a predicate is false" $ do
                pending

    where
        (ctx, ast) `shouldRender` txt = renderWith ctx ast `shouldBe` Right txt

        assertNoRender ctx ast =
            case renderWith ctx ast of
                Left _  -> True
                _       -> False
