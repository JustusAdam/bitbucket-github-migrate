{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
module Transfer where


import           API.Bitbucket
import           API.GitHub
import           ClassyPrelude
import           Control.Arrow             (second)
import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.Default
import           Data.Serialize            as Slz
import           Migrate.Internal.Types
import           Network.HTTP.Types.Status
import           Network.Wreq
import           System.Directory
import           System.FilePath
import           Util


data Transfer d r w = Transfer
    { tidentifier :: String
    , tacquire :: TransferMonad () d ()
    , tread    :: TransferMonad d r ()
    , twrite   :: TransferMonad (d, r) w ()
    }


data TransferWrapper = forall d r w. (Serialize d, Serialize r, Serialize w, Default d, Default r, Default w) => TransferWrapper (Transfer d r w)


transfers :: [TransferWrapper]
transfers = []


execTransferStep :: r -> w -> Maybe TransactionError -> TransferMonad r w a -> IO (Maybe TransactionError, w)
execTransferStep r w lastErr m = do
    mvar <- newMVar w
    err <- catch
        (flip fmap
            (runTransferMonad r mvar lastErr m)
            $ \case
                Right _ -> Nothing
                Left err -> Just err)
        (return . Just . Unexpected . show :: SomeException -> IO (Maybe TransactionError))
    mvarContent <- readMVar mvar
    return (err, mvarContent)


continueTransfer
    :: (Default d, Default r, Default w)
    => Transfer d r w
    -> TransferState d r w
    -> Maybe TransactionError
    -> IO (TransferState d r w, Maybe TransactionError)
continueTransfer Transfer{tacquire} (Initializing d) lastErr = do
    (err, newState) <- execTransferStep () d lastErr tacquire
    return $ (, err) $
        case err of
            Just e -> Initializing newState
            Nothing -> Reading d def
continueTransfer Transfer{tread} (Reading d r) lastErr = do
    (err, newState) <- execTransferStep d r lastErr tread
    return $ (, err) $
        case err of
            Just e -> Reading d newState
            Nothing -> Writing d newState def
continueTransfer Transfer{twrite} (Writing d r w) lastErr = do
    (err, newState) <- execTransferStep (d, r) w lastErr twrite
    return $ (, err) $
        case err of
            Just e -> Writing d r newState
            Nothing -> Finished d r newState
continueTransfer _ s e = return (s, e)


execTransfer
    :: (Default d, Default r, Default w, Serialize d, Serialize r, Serialize w)
    => String
    -> Transfer d r w
    -> IO (Maybe TransactionError)
execTransfer baseDirectory t = do
    state <- decode <$> readFile (baseDirectory </> tidentifier t <.> "log")
    case state of
        Left err -> return $ Just $ SerializationError err
        Right (prevErr, state') -> execTransferOn baseDirectory t prevErr state'


execTransferOn :: (Default d, Default r, Default w, Serialize d, Serialize r, Serialize w)
    => String
    -> Transfer d r w
    -> Maybe TransactionError
    -> TransferState d r w
    -> IO (Maybe TransactionError)
execTransferOn _ _ _ Finished{} = return Nothing
execTransferOn baseDirectory t@Transfer{tidentifier} prevErr state = do
    (newState, err) <- continueTransfer t state prevErr
    writeFile (baseDirectory </> tidentifier <.> "log") $ encode (err, newState)
    case err of
        Nothing -> execTransferOn baseDirectory t err newState
        Just _ -> return err


runTransferWrapper :: String -> TransferWrapper -> IO (Maybe TransactionError)
runTransferWrapper dir (TransferWrapper wrapper) = execTransfer dir wrapper


runTransfers :: String -> IO (Maybe TransactionError)
runTransfers dir = loop transfers
    where
        loop [] = return Nothing
        loop (x:xs) = do
            this <- runTransferWrapper dir x
            case this of
                Nothing -> loop xs
                Just _ -> return this
