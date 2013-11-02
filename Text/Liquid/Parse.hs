{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Text.Liquid.Parse where
    -- ( Template
    -- , TPart(..)
    -- , TPredicate(..)
    -- , parseTemplate
    -- ) where

import Control.Arrow
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

type Template = [TPart]

data TPart = TString Text
           | TFor Text Text Template
           | TIf TPredicate Template (Maybe Template)
           | TVar Text deriving (Show, Eq)

data TPredicate = TTruthy Text
                | TEquals Text Text
                | TNotEquals Text Text
                | TGreaterEquals Text Text
                | TLessEquals Text Text
                | TGreater Text Text
                | TLess Text Text deriving (Show, Eq)

parseTemplate :: Text -> Either String Template
parseTemplate = left show . parse template ""

template :: Parser Template
template = manyTill tpart eof

tpart :: Parser TPart
tpart = special <|> text

special :: Parser TPart
special =   try interpolation
        <|> try forLoop
        <|> ifStatement

interpolation :: Parser TPart
interpolation = fmap (TVar . T.pack)
              $ between (string "{{") (string "}}")
                        (stripped variable)

forLoop :: Parser TPart
forLoop = do
    openTag "for"
    elem <- variable
    stripped $ string "in"
    list <- variable
    many space
    string "%}"

    inner <- manyTill tpart (try $ endTag "for")

    return $ TFor (T.pack elem) (T.pack list) inner

ifStatement :: Parser TPart
ifStatement = do
    openTag "if"
    pred <- predicate
    many space
    string "%}"

    cons <- manyTill tpart (try $ endTag "if")

    -- TODO: Alternate
    return $ TIf pred cons Nothing

predicate :: Parser TPredicate
predicate =  try binaryPredicate
         <|> truthyPredicate

binaryPredicate :: Parser TPredicate
binaryPredicate = do
    lhs <- try quotedString <|> variable
    op  <- operator
    rhs <- try quotedString <|> variable

    return $
        case op of
            "==" -> TEquals (T.pack lhs) (T.pack rhs)
            "!=" -> TNotEquals (T.pack lhs) (T.pack rhs)
            ">=" -> TGreaterEquals (T.pack lhs) (T.pack rhs)
            "<=" -> TLessEquals (T.pack lhs) (T.pack rhs)
            ">"  -> TGreater (T.pack lhs) (T.pack rhs)
            "<"  -> TLess (T.pack lhs) (T.pack rhs)

    where
        quotedString = do
            c   <- oneOf "\'\""
            str <- manyTill anyChar $ char c

            return str

        operator =   stripped
                 $   try (string "==")
                 <|> try (string "!=")
                 <|> try (string ">=")
                 <|> try (string "<=")
                 <|> try (string ">")
                 <|> string "<"

truthyPredicate :: Parser TPredicate
truthyPredicate = fmap (TTruthy . T.pack) variable

text :: Parser TPart
text = fmap (TString . T.pack) $ manyTill anyToken eot

    where
        eot = lookAhead $   try (string "{{" >> return ())
                        <|> try (string "{%" >> return ())
                        <|> eof

variable :: Parser String
variable = many $   letter
                <|> char '_'
                <|> char '.'

openTag :: String -> Parser ()
openTag tg = do
    string "{%"
    stripped (string tg)

    return ()

endTag :: String -> Parser ()
endTag tg = do
    between (string "{%") (string "%}")
            (stripped $ string $ "end" ++ tg)

    return ()

stripped :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
stripped = between (many space) (many space)
