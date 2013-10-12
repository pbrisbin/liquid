{-# LANGUAGE OverloadedStrings #-}
module Text.Liquid.Parse where

import Control.Arrow
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

type Template = [TPart]

data TPart = TString Text
           | TVar Text deriving (Show, Eq)

parseTemplate :: Text -> Either String Template
parseTemplate = left show . parse template ""

template :: Parser Template
template = manyTill (special <|> text) eof'

-- TODO: Parse more structures
special :: Parser TPart
special = interpolation

interpolation :: Parser TPart
interpolation = do
    string "{{"
    variable <- manyTill anyChar (string "}}")

    return $ TVar (T.pack variable)

text :: Parser TPart
text = do
    value <- manyTill anyChar $ lookAhead special <|> eof'

    return $ TString (T.pack value)

eof' :: Parser TPart
eof' = eof >> return (TString "")
