{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Text.Liquid.Context
import Text.Liquid.Parse
import Text.Liquid.Render
import Text.Liquid

data Post = Post { postTitle :: Text }

instance ToJSON Post where
    toJSON (Post title) = object ["title" .= title]

data User = User
    { userName  :: Text
    , userAge   :: Int
    , userPosts :: [Post]
    }

instance ToJSON User where
    toJSON (User name age posts) =
        object ["user" .= object [ "name"  .= name
                                 , "age"   .= age
                                 , "posts" .= map toJSON posts
                                 ]]

main :: IO ()
main = hspec $ do
    describe "renderWith" $ do
        it "renders simple strings" $ do
            renderWith undefined [(TString "hello world")]
                `shouldBe` Right "hello world"

        it "renders variable interpolation" $ do
            let variableValue = "Foo" :: Text

            renderWith (object ["name" .= variableValue]) [(TVar "name")]
                `shouldBe` Right variableValue

        it "renders nested variables" $ do
            let pat = User
                    { userName  = "Pat"
                    , userAge   = 28
                    , userPosts = []
                    }

            renderWith (toJSON pat) [(TVar "user.name")]
                `shouldBe` Right "Pat"

            renderWith (toJSON pat) [(TVar "user.age")]
                `shouldBe` Right "28"

        it "renders a for loop" $ do
            let pat = User
                    { userName  = "Pat"
                    , userAge   = 0
                    , userPosts = [ Post "Post one"
                                  , Post "Post two"
                                  ]
                    }

            renderWith (toJSON pat) [(TFor "post" "user.posts"
                                            [ TString "\nTitle: "
                                            , TVar "post.title"
                                            , TString " by "
                                            , TVar "user.name"
                                            , TString "\n"
                                            ])]
                `shouldBe` Right "\nTitle: Post one by Pat\n\nTitle: Post two by Pat\n"
