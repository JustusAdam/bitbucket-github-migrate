{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
module Migrate.Transfer where


import           ClassyPrelude
import           Data.Default
import           Data.Serialize            as Slz
import           Migrate.Internal.Types


data Transfer d r w = Transfer
    { tidentifier :: String
    , tacquire    :: TransferMonad () d ()
    , tread       :: TransferMonad d r ()
    , twrite      :: TransferMonad (d, r) w ()
    }


data TransferWrapper
    = forall d r w. (Serialize d, Serialize r, Serialize w, Default d, Default r, Default w)
    => TransferWrapper (Transfer d r w)


transfers :: [TransferWrapper]
transfers = []


execTransferStep :: r -> w -> Maybe TransactionError -> TransferMonad r w a -> IO (Maybe TransactionError, w)
execTransferStep r w lastErr m = do
    mvar <- newMVar w
    err <- catch
        (fmap
            (\case
                Right _ -> Nothing
                Left err -> Just err)
            (runTransferMonad r mvar lastErr m))
        (return . Just . Unexpected . show :: SomeException -> IO (Maybe TransactionError))
    mvarContent <- readMVar mvar
    return (err, mvarContent)


continueTransfer
    :: (Default d, Default r, Default w)
    => Transfer d r w
    -> TransferState d r w
    -> Maybe TransactionError
    -> IO (TransferState d r w, Maybe TransactionError)
continueTransfer Transfer{tacquire} (Initializing d) = continueTransferInner Initializing (flip Reading def) () d tacquire
continueTransfer Transfer{tread} (Reading d r)       = continueTransferInner (Reading d) (flip (Writing d) def) d r tread
continueTransfer Transfer{twrite} (Writing d r w)    = continueTransferInner (Writing d r) (Finished d r) (d, r) w twrite
continueTransfer _ s = const $ return (s, Nothing)


continueTransferInner
    :: (state -> state')
    -> (state -> state')
    -> readable
    -> state
    -> TransferMonad readable state ignored
    -> Maybe TransactionError
    -> IO (state', Maybe TransactionError)
continueTransferInner onError onSuccess r w f lastErr =
    (\(err, newState) -> ((if isJust err then onError else onSuccess) newState , err))
    <$> execTransferStep r w lastErr f


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


execTransferOn
    :: (Default d, Default r, Default w, Serialize d, Serialize r, Serialize w)
    => String
    -> Transfer d r w
    -> Maybe TransactionError
    -> TransferState d r w
    -> IO (Maybe TransactionError)
execTransferOn _ _ _ Finished{} = return Nothing
execTransferOn baseDirectory t@Transfer{tidentifier} prevErr state = do
    (newState, err) <- continueTransfer t state prevErr
    writeFile (baseDirectory </> tidentifier <.> "log") $ encode (err, newState)
    maybe (execTransferOn baseDirectory t err newState) (return . return) err


runTransferWrapper :: String -> TransferWrapper -> IO (Maybe TransactionError)
runTransferWrapper dir (TransferWrapper wrapper) = execTransfer dir wrapper


runTransfers :: String -> IO (Maybe TransactionError)
runTransfers dir = loop transfers
    where
        loop [] = return Nothing
        loop (x:xs) = do
            this <- runTransferWrapper dir x
            maybe (loop xs) (return . return) this
