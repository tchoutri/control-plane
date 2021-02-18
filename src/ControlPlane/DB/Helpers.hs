module ControlPlane.DB.Helpers where

import Colourista.IO                (cyanMessage, redMessage, yellowMessage)
import Control.Exception            (throw)
import Control.Exception.Safe       (MonadCatch, try)
import Control.Monad.Trans.Control  (MonadBaseControl)
import Data.Pool                    (withResource)
import Data.Time                    (NominalDiffTime)
import Database.PostgreSQL.Simple   as PG (ConnectInfo, FromRow, Only (Only), Query, ToRow, close, connect)
import Database.PostgreSQL.Transact as PGT (DBT, execute, formatQuery, query, runDBTSerializable)

import ControlPlane.DB.Types        (ConnectionPool, InternalError (NotFound, TooManyResults), createPool)

data QueryNature = Select | Insert | Update | Delete
  deriving (Show, Eq)

mkPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO ConnectionPool
mkPool connectInfo subPools timeout connections = 
  createPool (connect connectInfo) close subPools timeout connections

queryMany :: (ToRow params, FromRow result, MonadIO m)
          => QueryNature -> Query -> params -> DBT m [result]
queryMany queryNature q params = do
    logQueryFormat queryNature q params
    PGT.query q params

queryOne :: (ToRow params, FromRow result, MonadIO m)
         => QueryNature -> Query -> params -> DBT m result
queryOne queryNature q params = do
  logQueryFormat queryNature q params
  result <- PGT.query q params
  pure $ listToOne result

listToOne :: [result] -> result
listToOne [r] = r
listToOne []  = throw NotFound
listToOne _   = throw TooManyResults

maybeToOnly :: Maybe result -> Only result
maybeToOnly (Just result) = Only result
maybeToOnly Nothing       = throw NotFound

execute :: (ToRow params, MonadIO m) => QueryNature -> Query -> params -> DBT m ()
execute queryNature q params = do
  logQueryFormat queryNature q params
  PGT.execute q params
  pure ()

logQueryFormat :: (ToRow params, MonadIO m) => QueryNature -> Query -> params -> DBT m ()
logQueryFormat queryNature q params = do
  msg <- PGT.formatQuery q params
  case queryNature of
    Select -> liftIO $ cyanMessage $ "[SELECT] " <> decodeUtf8 msg
    Update -> liftIO $ yellowMessage $ "[UPDATE] " <> decodeUtf8 msg
    Insert -> liftIO $ yellowMessage $ "[INSERT] " <> decodeUtf8 msg
    Delete -> liftIO $ redMessage $ "[DELETE] " <> decodeUtf8 msg

runDB :: (MonadCatch m, MonadBaseControl IO m) => ConnectionPool -> DBT m a -> m (Either InternalError a)
runDB pool action = try $ withResource pool $ runDBTSerializable action
