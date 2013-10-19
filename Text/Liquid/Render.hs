{-# LANGUAGE OverloadedStrings #-}
module Text.Liquid.Render where

import Data.Text (Text)
import Text.Liquid.Context
import Text.Liquid.Parse

import qualified Data.Text as T
import qualified Data.Map as M

renderWith :: Context -> Template -> Either String Text
renderWith ctx = fmap T.concat . mapM (renderPartWith ctx)

renderPartWith :: Context -> TPart -> Either String Text
renderPartWith ctx (TString txt) = Right txt
renderPartWith ctx (TVar txt) =
    let (p, ps) = T.breakOn "." txt
    in case M.lookup p (unContext ctx) of
        Nothing -> Left $ "Unknown value in context"
                        ++ "\n  missing value:   " ++ (T.unpack txt)
                        ++ "\n  current context: " ++ (show ctx)

        Just (CVar v) -> Right v
        Just (CInt i) -> Right (T.pack $ show i)

        Just (CSub ctx') -> renderPartWith ctx' (TVar $ T.drop 1 ps)
