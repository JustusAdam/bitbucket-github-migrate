module Util where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.List        (stripPrefix)
import           Data.Maybe


newtype TypeTag a b = TypeTag { unTTag :: b }

toTag = TypeTag


jsonOptions = defaultOptions { fieldLabelModifier = camelTo '_' . filter (/= '_') }


withDropPrefix prefix o = o { fieldLabelModifier = (fieldLabelModifier o) . (fromMaybe <$> Prelude.id <*> stripPrefix prefix) }
