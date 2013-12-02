# Liquid Haskell

Haskell implementation of Liquid templating by Shopify.

# Installation

```
$ git clone https://github.com/pbrisbin/liquid
$ cd liquid && cabal install
```

# Usage

Generally speaking, the function `liquid` takes an object with a valid 
`ToJSON` instance and template content as `Text`. It returns either 
`Left error-message` or `Right processed-template`.

How you come about this jsonify-able object or the textual template is 
your business, but here is an example of how I might do things:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Text (Text)
import Text.Liquid

import qualified Data.Text as T

data Post = Post { postTitle :: Text }

data User = User
    { userName  :: Text
    , userAge   :: Int
    , userPosts :: [Post]
    }

instance ToJSON Post where
    toJSON (Post title) = object ["title" .= title]

instance ToJSON User where
    toJSON (User name age posts) =
      object [ "name"  .= name
             , "age"   .= age
             , "posts" .= map toJSON posts
             ]

liquid (User "Pat" 28 [Post "Post one", Post "Post two"]) $
    T.unlines [ "Name: {{name}}"
              , "Age:  {{age}}"
              , "Posts:"
              , "{% for post in posts %}"
              , "  * {{post.title}}"
              , "{% endfor %}"
              ]
```

This may seem verbose as a standalone example, but in a framework-using 
web application (like Yesod), you'll likely already have models with 
`ToJSON` instances.

A more realistic use case may simply be:

```haskell
myHandler :: UserId -> Handler Html
myHandler userId = do
    user     <- get404 userId
    template <- T.readFile "templates/user.html"

    return . preEscapedToMarkup
           $ either errorHandler id
           $ liquid user template
```

## Testing

Running all specs:

```
$ ghc -isrc -itest -e main test/Spec.hs
```

Running one spec:

```
$ ghc -isrc -itest -e main test/Text/Liquid/RenderSpec.hs
```

Continuously run specs as files are edited:

```
$ gem install bundler
$ bundle
$ bundle exec guard
```
