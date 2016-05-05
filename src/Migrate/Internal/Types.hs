{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Migrate.Internal.Types
    ( TransferMonad
    , TransferState(..)
    , TransactionError(..)
    , getInformation, get, write, append, modify, getPreviousError
    , runTransferMonad
    ) where


import           ClassyPrelude
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import           Data.Serialize             (Serialize)
import           GHC.Generics


data TransactionError
    = ServiceError String
    | SystemError String
    | SerializationError String
    | Unexpected String
    deriving (Show, Eq, Ord, Generic)


instance Serialize TransactionError where


data TransferEnvironment env w = TransferEnvironment
    { readableEnv :: env
    , writableRef :: MVar w
    , prevError  :: Maybe TransactionError
    }


newtype TransferMonad environment written return =
    TransferMonad { unTransferMonad :: ExceptT
                                        TransactionError
                                        (ReaderT (TransferEnvironment environment written) IO)
                                        return
                  } deriving (Monad, Applicative, Functor, MonadIO)


runTransferMonad :: r -> MVar w -> Maybe TransactionError -> TransferMonad r w a -> IO (Either TransactionError a)
runTransferMonad r mvar exc = flip runReaderT (TransferEnvironment r mvar exc) . runExceptT . unTransferMonad


data TransferState initialData dataRead dataWritten
    = Initializing initialData
    | Reading initialData dataRead
    | Writing initialData dataRead dataWritten
    | Finished initialData dataRead dataWritten
    deriving (Eq, Ord, Show, Generic)


instance (Serialize d, Serialize r, Serialize w) => Serialize (TransferState d r w) where


getInformation :: TransferMonad r w r
getInformation = TransferMonad (readableEnv <$> lift ask)


get :: TransferMonad r w w
get = TransferMonad (lift ask >>= readMVar . writableRef)


write :: w -> TransferMonad r w ()
write w = TransferMonad (lift ask >>= void . flip swapMVar w . writableRef)


modify :: (w -> w) -> TransferMonad r w w
modify f = TransferMonad $ do
    mvar <- writableRef <$> lift ask
    modifyMVar mvar (\val -> let x = f val in return (x, x))


append :: Monoid w => w -> TransferMonad r w w
append d = modify $ flip mappend d


succeed :: a -> TransferMonad r w a
succeed = return


safeFail :: TransactionError -> TransferMonad r w a
safeFail = TransferMonad . throwE


getPreviousError :: TransferMonad r w (Maybe TransactionError)
getPreviousError = TransferMonad $ prevError <$> lift ask
