{-# LANGUAGE OverloadedStrings #-}
module Text.Liquid.Render (renderWith) where

import Text.Liquid.Context
import Text.Liquid.Parse

import qualified Data.Text as T
import qualified Data.Vector as V

renderWith :: Value -> Template -> Either String Text
renderWith ctx = fmap T.concat . mapM (renderPartWith ctx)

renderPartWith :: Value -> TPart -> Either String Text
renderPartWith _   (TString txt) = Right txt

renderPartWith ctx (TVar txt) =
    case lookupValue txt ctx of
        Just v  -> Right (renderValue v)
        Nothing -> Left $ "Value not found in context: " ++ T.unpack txt

renderPartWith ctx (TFor elm list template) =
    case lookupValue list ctx of
        Just (Array vector) -> renderVector ctx elm vector template
        _ -> Left $ "Array not found in context: " ++ T.unpack elm

renderPartWith ctx (TIf prd cons malt) =
    case (isTrue ctx prd, malt) of
        (True,  _       ) -> renderWith ctx cons
        (False, Just alt) -> renderWith ctx alt
        _                 -> return ""

renderVector :: Value        -- ^ The outer context
             -> Text         -- ^ The name to use for each element
             -> Vector Value -- ^ The vector of elements
             -> Template     -- ^ The template to render
             -> Either String Text
renderVector outer key list template =
    fmap (T.concat . V.toList) $ V.forM list $ \value ->
        let sub = outer `combineValues` object [key .= value]
        in renderWith sub template

renderValue :: Value -> Text
renderValue (String t)   = t
renderValue (Number n)   = T.pack $ show n
renderValue (Bool True)  = "true"
renderValue (Bool False) = "false"
renderValue Null         = ""
renderValue _            = "<json value>"

isTrue :: Value -> TPredicate -> Bool
isTrue ctx (TTruthy key) =
    case lookupValue key ctx of
        Just v  -> isValueTruthy v
        Nothing -> False

isTrue ctx (TEquals lhs rhs) =
    case (lookupValue lhs ctx, lookupValue rhs ctx) of
        (Just v1, Just v2) -> v1 == v2
        (Just v,  _      ) -> isValueEqual v rhs
        _                  -> False

isTrue _ _ = error "NotImplementedError"

isValueTruthy :: Value -> Bool
isValueTruthy (Object _) = True
isValueTruthy (Array v)  = not $ V.null v
isValueTruthy (String t) = t /= ""
isValueTruthy (Number n) = n /= 0
isValueTruthy (Bool b)   = b
isValueTruthy Null       = False

-- | There may be edge-cases here, but I *think* this works
isValueEqual :: Value -> Text -> Bool
isValueEqual v rhs = renderValue v == rhs
