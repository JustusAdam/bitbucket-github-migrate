{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module API.Bitbucket where


import           ClassyPrelude
import           Control.Exception         (SomeException)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8     as BS
import           Data.Maybe                (fromMaybe)
import           Data.Time
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Status (ok200)
import           Network.Wreq
import           Util


bitbucketBaseUrl :: String
bitbucketBaseUrl = "https://api.bitbucket.org/"


data SimpleLink = SimpleLink
    { href :: Text
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON SimpleLink where
    toJSON = genericToJSON jsonOptions

instance FromJSON SimpleLink where
    parseJSON = genericParseJSON jsonOptions


data Issue = Issue
    { id         :: Integer
    , title      :: Text
    , reporter   :: User
    , assignee   :: Maybe User
    , content    :: MarkupContent
    , createdOn  :: ZonedTime
    , updatedOn  :: ZonedTime
    , state      :: Text
    , kind       :: Text
    , priority   :: Text
    , version    :: Maybe Text
    , component  :: Maybe Component
    , milestone  :: Maybe Text
    , watches    :: Integer
    , votes      :: Integer
    , repository :: Repository
    , links      :: IssueLinks
    , type_      :: Text
    } deriving (Show, Generic)


instance ToJSON Issue where
    toJSON = genericToJSON jsonOptions

instance FromJSON Issue where
    parseJSON = genericParseJSON jsonOptions


data Component = Component
    { cname  :: Text
    , clinks :: SelfOnlyLink
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON Component where
    toJSON = genericToJSON $ withDropPrefix "c" jsonOptions

instance FromJSON Component where
    parseJSON = genericParseJSON $ withDropPrefix "c" jsonOptions


data IssueLinks = IssueLinks
    { self        :: SimpleLink
    , comments    :: SimpleLink
    , watch       :: SimpleLink
    , attachments :: SimpleLink
    , html        :: SimpleLink
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON IssueLinks where
    toJSON = genericToJSON jsonOptions

instance FromJSON IssueLinks where
    parseJSON = genericParseJSON jsonOptions


data Repository = Repository
    { name     :: Text
    , fullName :: Text
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON Repository where
    toJSON = genericToJSON jsonOptions

instance FromJSON Repository where
    parseJSON = genericParseJSON jsonOptions


data MarkupContent = MarkupContent
    { icraw    :: Text
    , icmarkup :: Text
    , ichtml   :: Text
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON MarkupContent where
    toJSON = genericToJSON $ withDropPrefix "ic" jsonOptions

instance FromJSON MarkupContent where
    parseJSON = genericParseJSON $ withDropPrefix "ic" jsonOptions


data User = User
    { username    :: Text
    , displayName :: Text
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON User where
    toJSON = genericToJSON jsonOptions

instance FromJSON User where
    parseJSON = genericParseJSON jsonOptions

data PagedRequest a = PagedRequest
    { values  :: [a]
    , size    :: Integer
    , page    :: Integer
    , pagelen :: Integer
    , next    :: Maybe Text
    } deriving (Show, Generic)

instance ToJSON a => ToJSON (PagedRequest a) where
    toJSON = genericToJSON jsonOptions

instance FromJSON a => FromJSON (PagedRequest a) where
    parseJSON = genericParseJSON jsonOptions


data CommentShortInfo = CommentShortInfo
    { commInfoId    :: Integer
    , commInfoLinks :: SelfOnlyLink
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON CommentShortInfo where
    toJSON = genericToJSON $ withDropPrefix "commeInfo" jsonOptions

instance FromJSON CommentShortInfo where
    parseJSON = genericParseJSON $ withDropPrefix "commInfo" jsonOptions


data Comment = Comment
    { commentLinks     :: CommentLinks
    , commentContent   :: MarkupContent
    , commentCreatedOn :: ZonedTime
    , commentUser      :: User
    , commentUpdatedOn :: Maybe ZonedTime
    , commentIssue     :: IssueShortInfo
    } deriving (Show, Generic)

instance ToJSON Comment where
    toJSON = genericToJSON $ withDropPrefix "comment" jsonOptions

instance FromJSON Comment where
    parseJSON = genericParseJSON $ withDropPrefix "comment" jsonOptions


data CommentLinks = CommentLinks
    { commLinkSelf :: SimpleLink
    , commLinkHtml :: SimpleLink
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON CommentLinks where
    toJSON = genericToJSON $ withDropPrefix "commLink" jsonOptions

instance FromJSON CommentLinks where
    parseJSON = genericParseJSON $ withDropPrefix "commLink" jsonOptions


data IssueShortInfo = IssueShortInfo
    { isiLinks      :: SelfOnlyLink
    , isiTitle      :: Text
    , isiId         :: Integer
    , isiRepository :: RepositoryShortInfo
    } deriving (Show, Generic)

instance ToJSON IssueShortInfo where
    toJSON = genericToJSON $ withDropPrefix "isi" jsonOptions

instance FromJSON IssueShortInfo where
    parseJSON = genericParseJSON $ withDropPrefix "isi" jsonOptions


data SelfOnlyLink = SelfOnlyLink
    { solSelf :: SimpleLink
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON SelfOnlyLink where
    toJSON = genericToJSON $ withDropPrefix "sol" jsonOptions

instance FromJSON SelfOnlyLink where
    parseJSON = genericParseJSON $ withDropPrefix "sol" jsonOptions


data RepositoryShortInfo = RepositoryShortInfo
    { rsiLinks    :: RepositoryShortInfoLinks
    , rsiType     :: Text
    , rsiName     :: Text
    , rsiFullName :: Text
    , rsiUUID     :: Text
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON RepositoryShortInfo where
    toJSON = genericToJSON $ withDropPrefix "rsi" jsonOptions

instance FromJSON RepositoryShortInfo where
    parseJSON = genericParseJSON $ withDropPrefix "rsi" jsonOptions


data RepositoryShortInfoLinks = RepositoryShortInfoLinks
    { rsilSelf   :: SimpleLink
    , rsilHtml   :: SimpleLink
    , rsilAvatar :: SimpleLink
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON RepositoryShortInfoLinks where
    toJSON = genericToJSON $ withDropPrefix "rsil" jsonOptions

instance FromJSON RepositoryShortInfoLinks where
    parseJSON = genericParseJSON $ withDropPrefix "rsil" jsonOptions

type ReqUrl a = TypeTag a String
type APIVersion a = TypeTag a String


class FromJSON result => BitbucketRequestable reqData result where
    getUrl :: reqData -> ReqUrl result
    apiVersion :: reqData -> APIVersion result


instance BitbucketRequestable Repository Issue where
    getUrl Repository {fullName = n} = toTag $ unpack n <> "/issues"
    apiVersion _ = toTag "2.0"


bitbucketGETPaged :: forall reqData result. BitbucketRequestable reqData result => reqData -> IO (Either String (PagedRequest result))
bitbucketGETPaged reqData =
    flip fmap
        (try $ get url)
        $ \case
            Left err -> Left $ show (err :: SomeException)
            Right r -> if r ^. responseStatus == ok200
                            then eitherDecode $ r ^. responseBody
                            else Left $ BS.unpack $ r ^. responseStatus . statusMessage
    where
        url = bitbucketBaseUrl <> unTTag (apiVersion reqData :: APIVersion result) <> "/" <> unTTag (getUrl reqData :: ReqUrl result)
