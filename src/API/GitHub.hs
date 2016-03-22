{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module API.GitHub where


import           ClassyPrelude
import           Control.Exception         (SomeException)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8     as BS
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text, unpack)
import           Network.HTTP.Types.Status (ok200)
import           Network.Wreq
import           Util


type PushUrl a = TypeTag a String


data WritableIssue = WritableIssue
    { wIssueTitle     :: Text
    , wIssueBody      :: Text
    , wIssueAssignee  :: Text
    , wIssueMilestone :: Integer
    , wIssueLabels    :: [Text]
    } deriving (Eq, Ord, Generic, Show)

instance ToJSON WritableIssue where
    toJSON = genericToJSON $ withDropPrefix "wIssue" jsonOptions
