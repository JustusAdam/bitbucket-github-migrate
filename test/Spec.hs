{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
import           API.GitHub
import           ClassyPrelude
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Network.Wreq
import           System.Directory
import           Test.Hspec


data Wrapper = forall a. (Show a, FromJSON a) => Wrapper (TypeTag a)
data TypeTag a = TypeTag


decodeTestingList :: [(Wrapper, String, String, String)]
decodeTestingList =
    [ (Wrapper (TypeTag :: TypeTag Issue), "decodes an issue", "test/issue-14.json", "https://api.github.com/repos/JustusAdam/mustache/issues/14")
    , (Wrapper (TypeTag :: TypeTag Repository), "decodes a repository", "test/mustache.json", "https://api.github.com/repos/JustusAdam/mustache") ]


testOneDecode :: (Wrapper, String, FilePath, String) -> Spec
testOneDecode (Wrapper tag, line, file, url) = do
    raw <- runIO getFile
    it line $ ed tag raw
  where
    ed :: forall a. (FromJSON a, Show a) => TypeTag a -> BS.ByteString -> Expectation
    ed _ raw = (eitherDecode raw  :: Either String a) `shouldSatisfy` isRight

    getFile = do
        exists <- doesFileExist file
        if exists
            then readFile file
            else do
                res <- get url
                let body = res ^. responseBody
                writeFile file body
                return body


isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False


githubJSONSpec :: Spec
githubJSONSpec =
    describe "Json decoding" $
        mapM_ testOneDecode decodeTestingList


main :: IO ()
main = hspec githubJSONSpec
