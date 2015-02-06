{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Text.Liquid.Parse
    ( Template
    , TPart(..)
    , TPredicate(..)
    , parseTemplate
    ) where

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
parseTemplate = left show . parse (manyTill tpart $ try eof) ""

tpart :: Parser TPart
tpart = try outputTag <|> logicTag <|> textPart

-- TODO: filters
outputTag :: Parser TPart
outputTag = fmap TVar $ within "{{" "}}" variable

logicTag :: Parser TPart
logicTag = try forLoop <|> ifStatement

textPart :: Parser TPart
textPart = fmap TString $ textUntil eot

    where
        eot = lookAhead
            $   try (string "{{" >> return ())
            <|> try (string "{%" >> return ())
            <|> eof

forLoop :: Parser TPart
forLoop = do
    (e, list) <- tagWith "for" eachIn
    inner     <- manyTill tpart $ try endFor

    return $ TFor e list inner

    where
        eachIn = do
            e <- variable
            _ <- many1 space
            _ <- string "in"
            _ <- many1 space
            es <- variable

            return (e, es)

        endFor = endTag "for"

ifStatement :: Parser TPart
ifStatement = do
    p <- tagWith "if" $   try binaryPredicate
                      <|> truthyPredicate

    (cons, alt) <-   try withAlternative
                 <|> consequentOnly

    return $ TIf p cons alt

    where
        consequentOnly = do
            c <- manyTill tpart $ try endIf

            return (c, Nothing)

        withAlternative = do
            c <- manyTill tpart $ try elseTag
            a <- manyTill tpart $ try endIf

            return (c, Just a)

        elseTag = do
            _ <- tag (string $ "else")

            return ()

        endIf = endTag "if"

binaryPredicate :: Parser TPredicate
binaryPredicate = do
    lhs  <- variable
    mkOp <- operator
    rhs  <-   try quotedString
          <|> try bareNumber
          <|> variable

    return $ mkOp lhs rhs

    where
        -- TODO: escaped quotes
        quotedString = do
            c   <- oneOf "\'\""
            str <- manyTill anyChar $ char c

            return $ T.pack str

        bareNumber = fmap T.pack $ many1 digit

        operator = stripped
                 $   try (string "==" >> return TEquals)
                 <|> try (string "!=" >> return TNotEquals)
                 <|> try (string ">=" >> return TGreaterEquals)
                 <|> try (string "<=" >> return TLessEquals)
                 <|> try (string ">"  >> return TGreater)
                 <|>     (string "<"  >> return TLess)

truthyPredicate :: Parser TPredicate
truthyPredicate = fmap TTruthy $ variable

variable :: Parser Text
variable = fmap T.pack $ many $ letter <|> oneOf "_."

endTag :: String -> Parser ()
endTag tg = tag (string $ "end" ++ tg) >> return ()

tagWith :: Stream s m Char => String -> ParsecT s u m a -> ParsecT s u m a
tagWith tg p = tag $ string tg >> many1 space >> p

tag :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
tag p = between (string "{%") (string "%}") (stripped p)

stripped :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
stripped = between spaces spaces

within :: Stream s m Char
       => String
       -> String
       -> ParsecT s u m a
       -> ParsecT s u m a
within a b = between (string a >> spaces) (spaces >> string b)

textUntil :: Stream s m Char => ParsecT s u m end -> ParsecT s u m Text
textUntil p = fmap T.pack $ manyTill anyToken p
