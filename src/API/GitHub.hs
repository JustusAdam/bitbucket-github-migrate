{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module API.GitHub where


import           ClassyPrelude
import           Control.Exception         (SomeException)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8     as BS
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text, unpack)
import           Data.Time
import           Network.HTTP.Types.Status (ok200)
import           Network.Wreq
import           Util


data WritableIssue = WritableIssue
    { wIssueTitle     :: Text
    , wIssueBody      :: Text
    , wIssueAssignee  :: Text
    , wIssueMilestone :: Integer
    , wIssueLabels    :: [Text]
    } deriving (Eq, Ord, Generic, Show)

instance ToJSON WritableIssue where
    toJSON = genericToJSON $ withDropPrefix "wIssue" jsonOptions


data Repository = Repository
    { repoId               :: Integer
    , repoOwner            :: User
    , repoName             :: Text
    , repoFullName         :: Text
    , repoDescription      :: Text
    , repoPrivate          :: Bool
    , repoFork             :: Bool
    , repoUrl              :: Text
    , repoHtmlUrl          :: Text
    , repoArchiveUrl       :: Text
    , repoAssigneesUrl     :: Text
    , repoBlobsUrl         :: Text
    , repoBranchesUrl      :: Text
    , repoCloneUrl         :: Text
    , repoCollaboratorsUrl :: Text
    , repoCommentsUrl      :: Text
    , repoCommitsUrl       :: Text
    , repoCompareUrl       :: Text
    , repoIssuesUrl        :: Text
    , repoHomepage         :: Maybe Text
    , repoLanguage         :: Maybe Text
    , repoForksCount       :: Integer
    , repoStargazersCount  :: Integer
    , repoWatchersCount    :: Integer
    , repoSize             :: Integer
    , repoDefaultBranch    :: Text
    , repoOpenIssuesCount  :: Integer
    , repoHasIssues        :: Bool
    , repoHasPages         :: Bool
    , repoHasWiki          :: Bool
    , repoHasDownloads     :: Bool
    , repoPushedAt         :: ZonedTime
    , repoCreatedAt        :: ZonedTime
    , repoUpdatedAt        :: ZonedTime
    , repoSubscribersCount :: Integer
    , repoParent           :: Maybe Repository
    , repoSource           :: Maybe Repository
    , repoOrganization     :: Maybe Organization
    , repoPermissions      :: Maybe RepoPermissions
    } deriving (Show, Generic)


type Organization = User


instance FromJSON Repository where
    parseJSON = genericParseJSON $ withDropPrefix "repo" jsonOptions

instance ToJSON Repository where
    toJSON = genericToJSON $ withDropPrefix "repo" jsonOptions


data RepoPermissions = RepoPermissions
    { rPermAdmin :: Bool
    , rPermPush  :: Bool
    , rPermPull  :: Bool
    } deriving (Show, Generic)

instance FromJSON RepoPermissions where
    parseJSON = genericParseJSON $ withDropPrefix "rPerm" jsonOptions

instance ToJSON RepoPermissions where
    toJSON = genericToJSON $ withDropPrefix "rPerm" jsonOptions


data User = User
    { userLogin             :: Text
    , userId                :: Integer
    , userAvatarUrl         :: Text
    , userGravatarID        :: Text
    , userUrl               :: Text
    , userHtmlUrl           :: Text
    , userFollowersUrl      :: Text
    , userFollowingUrl      :: Text
    , userGistsUrl          :: Text
    , userStarredUrl        :: Text
    , userSubscriptionsUrl  :: Text
    , userOrganizationsUrl  :: Text
    , userReposUrl          :: Text
    , userEventsUrl         :: Text
    , userReceivedEventsUrl :: Text
    , userType              :: Text
    , userSiteAdmin         :: Bool
    } deriving (Show, Generic)


instance FromJSON User where
    parseJSON = genericParseJSON $ withDropPrefix "user" jsonOptions

instance ToJSON User where
    toJSON = genericToJSON $ withDropPrefix "user" jsonOptions


data UserType = UTUser | UTOrganization deriving (Show, Generic)


instance ToJSON UserType where
    toJSON = genericToJSON $ jsonEnumOptions { constructorTagModifier =  (fromMaybe <$> id <*> stripPrefix "UT") }

instance FromJSON UserType where
    parseJSON = genericParseJSON $ jsonEnumOptions { constructorTagModifier =  (fromMaybe <$> id <*> stripPrefix "UT") }


data CreateRepository = CreateRepository
    { createRepoName              :: Text
    , createRepoDescription       :: Maybe Text
    , createRepoHomepage          :: Maybe Text
    , createRepoPrivate           :: Bool
    , createRepoHasIssues         :: Bool
    , createRepoHasWiki           :: Bool
    , createRepoHasDownloads      :: Bool
    , createRepoTeamId            :: Maybe Integer
    , createRepoAutoInit          :: Bool
    , createRepoGitignoreTemplate :: Text
    , createRepoLicenseTemplate   :: Text
    } deriving (Show, Generic)


instance FromJSON CreateRepository where
    parseJSON = genericParseJSON $ withDropPrefix "createRepo" jsonOptions

instance ToJSON CreateRepository where
    toJSON = genericToJSON $ withDropPrefix "createRepo" jsonOptions


data Issue = Issue
    { issueId            :: Integer
    , issueUrl           :: Text
    , issueRepositoryUrl :: Text
    , issueLabelsUrl     :: Text
    , issueCommentsUrl   :: Text
    , issueEventsUrl     :: Text
    , issueHtmlUrl       :: Text
    , issueNumber        :: Integer
    , issueState         :: IssueState
    , issueTitle         :: Text
    , issueBody          :: Text
    , issueUser          :: User
    , issueLabels        :: [Label]
    , issueAssignee      :: Maybe User
    , issueMilestone     :: Maybe Milestone
    , issueLocked        :: Bool
    , issueComments      :: Integer
    , issueClosedAt      :: Maybe ZonedTime
    , issueCreatedAt     :: ZonedTime
    , issueUpdatedAt     :: ZonedTime
    } deriving (Show, Generic)


instance FromJSON Issue where
    parseJSON = genericParseJSON $ withDropPrefix "issue" jsonOptions

instance ToJSON Issue where
    toJSON = genericToJSON $ withDropPrefix "issue" jsonOptions


data IssueState = IssueOpen | IssueClosed deriving (Show, Generic)


instance ToJSON IssueState where
    toJSON = genericToJSON $ lcPrefixedEnumOptions "Issue"

instance FromJSON IssueState where
    parseJSON = genericParseJSON $ lcPrefixedEnumOptions "Issue"


data Label = Label
    { labelUrl   :: Text
    , labelName  :: Text
    , labelColor :: Text
    } deriving (Show, Generic)

instance FromJSON Label where
    parseJSON = genericParseJSON $ withDropPrefix "label" jsonOptions

instance ToJSON Label where
    toJSON = genericToJSON $ withDropPrefix "label" jsonOptions

data Milestone = Milestone
    { milestoneUrl          :: Text
    , milestoneHtmlUrl      :: Text
    , milestoneLabelsUrl    :: Text
    , milestoneId           :: Integer
    , milestoneNumber       :: Integer
    , milestoneState        :: MilestoneState
    , milestoneTitle        :: Text
    , milestoneDescription  :: Text
    , milestoneCreator      :: User
    , milestoneOpenIssues   :: Integer
    , milestoneClosedIssues :: Integer
    , milestoneCreatedAt    :: ZonedTime
    , milestoneUpdatedAt    :: ZonedTime
    , milestoneClosedAt     :: Maybe ZonedTime
    , milestoneDueOn        :: Maybe ZonedTime
    } deriving (Show, Generic)


instance FromJSON Milestone where
    parseJSON = genericParseJSON $ withDropPrefix "milestone" jsonOptions

instance ToJSON Milestone where
    toJSON = genericToJSON $ withDropPrefix "milestone" jsonOptions


data MilestoneState = MilestoneOpen | MilestoneClosed deriving (Show, Generic)

instance ToJSON MilestoneState where
    toJSON = genericToJSON $ lcPrefixedEnumOptions "Milestone"

instance FromJSON MilestoneState where
    parseJSON = genericParseJSON $ lcPrefixedEnumOptions "Milestone"
