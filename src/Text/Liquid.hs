module Text.Liquid (liquid) where

import Control.Monad ((<=<))
import Text.Liquid.Context
import Text.Liquid.Parse
import Text.Liquid.Render

liquid :: ToJSON a => a -> Text -> Either String Text
liquid ctx = renderWith (toJSON ctx) <=< parseTemplate
