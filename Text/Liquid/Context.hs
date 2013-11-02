{-# LANGUAGE OverloadedStrings #-}
module Text.Liquid.Context
    ( lookupValue
    , combineValues
    , module Data.Aeson
    , module Data.Text
    , module Data.Vector
    ) where

import Data.Aeson
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

-- | Dot-notation access into an Object
lookupValue :: Text -> Value -> Maybe Value
lookupValue key (Object hm) = do
    let (key', rest) = T.breakOn "." key

    case (HM.lookup key' hm, rest) of
        (v, "") -> v
        ((Just o@(Object _)), _ ) -> lookupValue (T.drop 1 rest) o
        _ -> Nothing

lookupValue _ _ = Nothing

-- | When both arguments are Objects, return a new Object with the keys
--   and values from both, otherwise return the first argument as is.
--   When keys are duplicated the second argument's remain.
combineValues :: Value -> Value -> Value
combineValues (Object hm1) (Object hm2) = Object $ HM.union hm2 hm1
combineValues v _ = v
