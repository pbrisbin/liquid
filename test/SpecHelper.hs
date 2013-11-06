{-# LANGUAGE OverloadedStrings #-}
module SpecHelper
    ( Post(..)
    , User(..)
    , posts
    , user
    , shouldParseTo
    , shouldRender
    , assertNoParse
    , assertNoRender
    , module Test.Hspec
    , module Text.Liquid.Context
    , module Text.Liquid.Render
    , module Text.Liquid.Parse
    ) where

import Data.Aeson
import Test.Hspec
import Text.Liquid.Context
import Text.Liquid.Render
import Text.Liquid.Parse

data Post = Post Text

instance ToJSON Post where
    toJSON (Post title) = object ["title" .= title]

posts :: [Post]
posts = [ Post "Post one"
        , Post "Post two"
        , Post "Post three"
        ]

data User = User
    { userName  :: Text
    , userAge   :: Int
    , userPosts :: [Post]
    }

user :: User
user = User "Pat" 28 posts

instance ToJSON User where
    toJSON (User name age posts') =
        object ["user" .= object [ "name"  .= name
                                 , "age"   .= age
                                 , "posts" .= map toJSON posts'
                                 ]]

shouldParseTo :: Text -> Template -> Expectation
str `shouldParseTo` ast = parseTemplate str `shouldBe` Right ast

shouldRender :: (Value, Template) -> Text -> Expectation
(ctx, ast) `shouldRender` txt = renderWith ctx ast `shouldBe` Right txt

assertNoParse :: Text -> Bool
assertNoParse str =
    case parseTemplate str of
            Left _ -> True
            _      -> False

assertNoRender :: Value -> Template -> Bool
assertNoRender ctx ast =
    case renderWith ctx ast of
        Left _  -> True
        _       -> False
