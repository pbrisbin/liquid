module Text.Liquid where

import Control.Monad
import Control.Monad.Instances
import Data.Text (Text)
import Text.Liquid.Context
import Text.Liquid.Parse
import Text.Liquid.Render

liquid :: [(Text, CValue)] -> Text -> Either String Text
liquid ctx = renderWith (fromList ctx) <=< parseTemplate
