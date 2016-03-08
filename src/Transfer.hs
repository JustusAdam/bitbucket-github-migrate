{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase, NamedFieldPunning, MultiWayIf #-}
module Transfer where


import           Control.Monad
import qualified Data.ByteString  as BS
import           Data.Default
import           Data.Serialize   as Slz
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           Util
import API.GitHub
import API.BitBucket
import           Network.HTTP.Types.Status 
import           Network.Wreq
import Control.Arrow (second)


data Transfer = Transfer
    { initialize :: IO ()
    , continue   :: IO (Either String ())
    , isFinished :: IO Bool
    }

class (Default a, Serialize a) => TransferData a where
    stateFileName :: TypeTag a FilePath


dummyTransfer :: Transfer
dummyTransfer = Transfer (return ()) (return $ Right ()) (return True)

transferLogBaseFolder :: FilePath
transferLogBaseFolder = "transfer-log"

readState :: forall a. TransferData a => IO (Either String a)
readState = do
    exists <- doesFileExist filename
    if exists
        then do
            file <- BS.readFile filename
            return $ decode file
        else
            return $ Right def
    where
        filename = transferLogBaseFolder </> unTTag (stateFileName :: TypeTag a FilePath) <.> "state"

writeState :: forall a. TransferData a => a -> IO ()
writeState data_ =
    BS.writeFile (transferLogBaseFolder </> unTTag (stateFileName :: TypeTag a FilePath) <.> "state") $ encode data_


transferChain :: [Transfer]
transferChain =
    [ milestoneTransfer
    , issueTransfer
    ]


milestoneTransfer :: Transfer
milestoneTransfer = dummyTransfer


data TransferOperation
    = Reading
    | Processing
    | Writing
    deriving (Eq, Ord, Show, Generic)

instance Serialize TransferOperation where

data TransferState position
    = NotStarted
    | Stopped position
    | Finished
    deriving (Eq, Ord, Show, Generic)

instance Serialize position => Serialize (TransferState position) where

data IssueTransferData = IssueTransferData
    { issueTransferState :: TransferState TransferOperation
    , fullRepositoryName :: String
    } deriving (Eq, Ord, Show, Generic)

instance Serialize IssueTransferData where

instance Default IssueTransferData where
    def = IssueTransferData NotStarted

instance TransferData IssueTransferData where
    stateFileName = toTag "issues"


data ProcessingError 
    = ReadError ServiceError
    | WriteError ServiceError
    | ProcessingError String
    | OtherError String
    deriving (Eq, Ord, Show, Generic)

data ServiceError 
    = RateLimitExceeded
    | Unreachable
    | InvalidRequest
    | Other String
    deriving (Eq, Ord, Show, Generic) 


data IssueTransferPosition = IssueTransferPosition
    { lastPageRead :: Integer
    , totalPagesNr :: Integer
    , nextPage :: Maybe String
    , pendingWrites :: [WritableIssue] 
    } deriving (Eq, Ord, Show, Generic)


issueTransfer :: Transfer
issueTransfer = Transfer { initialize = initIssueTransfer
                         , continue = continueIssueTransfer
                         , isFinished = isIssueTransferFinished
                         }

initIssueTransfer :: IO ()
initIssueTransfer = return ()

isIssueTransferFinished :: IO Bool
isIssueTransferFinished = do
    state <- readState
    return $ case state of
                Right (IssueTransferData Finished) -> True
                _ -> False


convertIssues :: [Issue] -> [WritableIssue]
convertIssues _ = []


continueIssueTransfer :: IO (Either String ())
continueIssueTransfer = do 
    state <- readState
    case state of
        Right state -> do
            processed <- case issueTransferState state of
                            Finished -> return $ (state, Right ())
                            (Stopped position) ->
                                mapLeft (second Just) <$> continueIssueTransfer' position
                            NotStarted -> startIssueTransfer (fullRepositoryName state)
            case processed of
                Left (err, mnewPosition) -> do
                    case mnewPosition of
                        Just newPosition ->
                            writeState $ state { issueTransferState = Stopped newPosition }
                        Nothing -> return ()
                    return $ Left $ show err
                Right _ -> do
                    writeState $ state { issueTransferState = Finished }
                    return $ Right ()
        Left err -> return $ Left err


writeGitHub :: ToJSON a => String -> a -> IO (Either ServiceError ())
writeGitHub url payload =
    try (post url (toJSON payload)) >>= 
    \case 
        Left err -> return $ Left $ Other $ show (err :: SomeException)
        Right resp -> 
            let 
                status = resp ^. responseStatus
            in return if
                | status == ok200 -> return ()
                | status ^. statusCode == 422 -> InvalidRequest  
                | otherwise -> Left $ Other $ unpack $ resp ^. responseStatus . statusMessage


writeIssue :: String -> WritableIssue -> IO (Either ServiceError ())
writeIssue repoName issue = do
    writeGitHub (printf "https://api.github.com/repos/%v/issues" repoName)
        


getNextIssuePage :: String -> IO (Either ProcessingError (PagedRequest Issue))
getNextIssuePage url =
    resp <- try $ get url
    case resp of
        Left err -> return $ Left $ show (err :: SomeException)
        Right respData -> do
            if respData ^. responseStatus == ok200
                then
                    eitherDecode $ respData ^. responseBody
                else return $ Left $ B.unpack (respData ^. responseStatus . statusMessage)


issuePageToPosition :: PagedRequest Issue -> IssueTransferPosition
issuePageToPosition reqRes =
    IssueTransferPosition
        { lastPageRead = page reqRes 
        , totalPagesNr = size reqRes
        , nextPage = next reqRes
        , pendingWrites = convertIssues $ values reqRes
        }


startIssueTransfer :: String -> IO (Either (ProcessingError, Maybe IssueTransferPosition) ())
startIssueTransfer repoName =
    getNextIssuePage (printf "https://api.bitbucket.org/2.0/repositories/%v/issues" repoName) >>= \case
        Left err -> return $ InitializationFailed err
        Right reqRes -> 
            mapLeft (second Just) <$> continueIssueTransfer' (issuePageToPosition reqRes)


continueIssueTransfer' :: IssueTransferPosition -> IO (Either (ProcessingError, IssueTransferPosition) ())
continueIssueTransfer' position =
    issuesWritten <- writeIssues
    case issuesWritten of
        Right { nextPage = Nothing, pendingWrites = [] } -> return $ Right ()
        Right { nextPage, pendingWrites = [] } = do 
            newPosition <- fmap issuePageToPosition <$> getNextIssuePage nextPage)
            case newPosition of
                Left _ -> newPosition
                Right position -> continueIssueTransfer position
        Right _ -> return $ Left (OtherError "not all issues have been written, no idea why", issuesWritten)
        _ -> issuesWritten
    where
        writeIssues = do
            
