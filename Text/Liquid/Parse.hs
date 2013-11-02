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
    lhs  <- variable
    mkOp <- operator
    rhs  <-   try quotedString
          <|> try bareNumber
          <|> variable

    return $ mkOp (T.pack lhs) (T.pack rhs)

    where
        quotedString = do
            c   <- oneOf "\'\""
            str <- manyTill anyChar $ char c

            return str

        bareNumber = many1 digit

        operator = do
            many space
            op <-   try (string "==" >> return TEquals)
                <|> try (string "!=" >> return TNotEquals)
                <|> try (string ">=" >> return TGreaterEquals)
                <|> try (string "<=" >> return TLessEquals)
                <|> try (string ">"  >> return TGreater)
                <|>     (string "<"  >> return TLess)
            many space

            return op

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
