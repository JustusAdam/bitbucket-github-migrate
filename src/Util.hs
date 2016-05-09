module Util where


import           ClassyPrelude
import           Data.Aeson.Types


newtype TypeTag a b = TypeTag { unTTag :: b }

toTag :: b -> TypeTag a b
toTag = TypeTag


jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelTo '_' . filter (/= '_') }


jsonEnumOptions :: Options
jsonEnumOptions = jsonOptions { allNullaryToStringTag = True }


lcPrefixedEnumOptions :: String -> Options
lcPrefixedEnumOptions prefix = jsonEnumOptions { constructorTagModifier = toLower . (stripPrefix prefix >>= flip fromMaybe) }


withDropPrefix :: String -> Options -> Options
withDropPrefix prefix o = o { fieldLabelModifier = fieldLabelModifier o . (stripPrefix prefix >>= flip fromMaybe) }


withDropConstructorPrefix :: String -> Options -> Options
withDropConstructorPrefix prefix o = o { constructorTagModifier = constructorTagModifier o . (stripPrefix prefix >>= flip fromMaybe) }


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right a) = return a
