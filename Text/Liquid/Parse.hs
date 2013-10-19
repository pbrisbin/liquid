{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Text.Liquid.Parse where

import Control.Arrow
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

type Template = [TPart]

data TPart = TString Text
           | TFor Text Text Template
           | TVar Text deriving (Show, Eq)

parseTemplate :: Text -> Either String Template
parseTemplate = left show . parse template ""

template :: Parser Template
template = templateTill eof'

templateTill p = manyTill (interpolation <|> text) p

-- special :: Parser TPart
-- special = try interpolation <|> try forLoop

interpolation = output (many $ letter <|> underscore <|> dot)

output p = between
    (string "{{")
    (string "}}")
    (stripped p)

tag p inner = do
    string "{%"
    (t,rest) <- p
    string "%}"
    inner <- inner
    string "{%"
    stripped $ string "end" ++ t
    string "%}"

    return t rest inner

identifier :: Parser String
identifier = many $ letter <|> underscore

variable :: Parser String
variable = many $ letter <|> underscore <|> dot

underscore :: Parser Char
underscore = char '_'

dot :: Parser Char
dot = char '.'

text :: Parser TPart
text = do
    value <- manyTill anyToken
           $ ended <|> eof'

    return $ TString (T.pack value)

    where
        ended = lookAhead (output anyToken) <|> (tag anyToken anyToken)

stripped :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
stripped = between (many space) (many space)

eof' :: Parser TPart
eof' = eof >> return (TString "")
