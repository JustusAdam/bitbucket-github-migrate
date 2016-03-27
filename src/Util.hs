module Util where


import           ClassyPrelude
import           Data.Aeson.Types


newtype TypeTag a b = TypeTag { unTTag :: b }

toTag :: b -> TypeTag a b
toTag = TypeTag


jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelTo '_' . filter (/= '_') }


withDropPrefix :: String -> Options -> Options
withDropPrefix prefix o = o { fieldLabelModifier = fieldLabelModifier o . (fromMaybe <$> id <*> stripPrefix prefix) }


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right a) = return a
