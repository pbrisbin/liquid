module Text.Liquid.Context where

import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Map as M

data CValue = CVar Text
            | CInt Int
            | CArray [CValue]
            | CSub Context deriving Show

newtype Context = Context
    { unContext :: M.Map Text CValue }

instance Show Context where
    show = show . map T.unpack . M.keys . unContext

fromList :: [(Text, CValue)] -> Context
fromList = Context . M.fromList
