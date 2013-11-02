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
template = manyTill tpart eof'

tpart :: Parser TPart
tpart = try special <|> text

special :: Parser TPart
special = try interpolation <|> forLoop

interpolation :: Parser TPart
interpolation = do
    str <- between (string "{{")
                   (string "}}")
                   (stripped variable)

    return $ TVar (T.pack str)

    where

forLoop :: Parser TPart
forLoop = do
    openFor
    many space
    elem <- variable
    stripped $ string "in"
    list <- variable
    many space
    string "%}"

    inner <- manyTill tpart (try endFor)

    return $ TFor (T.pack elem) (T.pack list) inner

    where
        openFor = do
            string "{%"
            many space
            string "for"

text :: Parser TPart
text = do
    value <- manyTill anyToken $ ended

    return $ TString (T.pack value)

    where
        ended = lookAhead $ try special <|> try ender <|> eof'

variable :: Parser String
variable = many $ letter <|> char '_' <|> char '.'

endFor :: Parser ()
endFor = do
    between (string "{%")
            (string "%}")
            (stripped $ string "endfor")

    return ()

-- | Result of this parser should always be discarded. It returns an
--   empty TPart so it can be used a section delimiter in text.
ender :: Parser TPart
ender = do
    endFor -- <|> endWhatever

    return $ TString ""

stripped :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
stripped = between (many space) (many space)

eof' :: Parser TPart
eof' = eof >> return (TString "")
