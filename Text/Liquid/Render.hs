{-# LANGUAGE OverloadedStrings #-}
module Text.Liquid.Render where

import Data.Text (Text)
import Data.Monoid
import Text.Liquid.Context
import Text.Liquid.Parse

import qualified Data.Text as T
import qualified Data.Map as M

renderWith :: Context -> Template -> Either String Text
renderWith ctx = concatMap' (renderPartWith ctx)

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

renderPartWith ctx (TFor elem list template) =
    case M.lookup list (unContext ctx) of
        Just (CArray list') -> renderEach elem list' template

        _ -> Left $ "Unknown list in context"
                  ++ "\n  missing value:   " ++ (T.unpack list)
                  ++ "\n  current context: " ++ (show ctx)

    where
        renderEach :: Text -> [CValue] -> Template -> Either String Text
        renderEach key list template =
            concatMap' (renderItem key template) list

        renderItem :: Text -> Template -> CValue -> Either String Text
        renderItem key template value =
            renderWith (fromList [(key, value)]) template

concatMap' :: (Monad m, Functor m, Monoid b) => (a -> m b) -> [a] -> m b
concatMap' f xs = fmap mconcat $ mapM f xs
