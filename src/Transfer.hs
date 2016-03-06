{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Transfer where


import           Control.Monad
import qualified Data.ByteString  as BS
import           Data.Default
import           Data.Serialize   as Slz
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           Util


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
    } deriving (Eq, Ord, Show, Generic)

instance Serialize IssueTransferData where

instance Default IssueTransferData where
    def = IssueTransferData NotStarted

instance TransferData IssueTransferData where
    stateFileName = toTag "issues"


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

continueIssueTransfer :: IO (Either String ())
continueIssueTransfer = return $ return ()
