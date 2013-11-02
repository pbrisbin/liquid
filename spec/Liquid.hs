{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import Test.Hspec

import Text.Liquid.Context
import Text.Liquid
import Text.Shakespeare.Text (st)

main :: IO ()
main = hspec $ do
    describe "liquid" $ do
        it "works" $ do
            let pat = fromList [("name", CVar "Pat"), ("age", CInt 28)]

                posts = [ CSub $ fromList [("title", CVar "Post one")]
                        , CSub $ fromList [("title", CVar "Post two")]
                        ]

                template = [st|
Name: {{user.name}}
Age: {{user.age}}
Posts:
{% for post in posts %}
    {{post.title}}
{% endfor %}
|]

                expected = [st|
Name: Pat
Age: 28
Posts:

    Post one

    Post two

|]

            liquid [("user", CSub $ pat), ("posts", CArray $ posts)] template
                `shouldBe` Right expected
