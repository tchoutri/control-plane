module ControlPlane.DB.Helpers where

import Colourista.IO                (cyanMessage)
import Control.Exception            (throw)
import Control.Exception.Safe       (MonadCatch, try)
import Control.Monad.Trans.Control  (MonadBaseControl)
import Data.Pool                    (withResource)
import Data.Time                    (NominalDiffTime)
import Database.PostgreSQL.Simple   as PG
import Database.PostgreSQL.Transact as PGT
import Servant                      (NoContent (..))

import ControlPlane.DB.Types

mkPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO ConnectionPool
mkPool connectInfo subPools timeout connections = 
  createPool (connect connectInfo) close subPools timeout connections

queryMany :: (ToRow params, FromRow result, MonadIO m) => Query -> params -> DBT m [result]
queryMany q params = do
    logQueryFormat q params
    PGT.query q params

queryOne :: (ToRow params, FromRow result, MonadIO m) => Query -> params -> DBT m (Only result)
queryOne q params = do
  logQueryFormat q params
  result <- PGT.query q params
  pure $ transformToOnly result

transformToOnly :: [result] -> Only result
transformToOnly [r] = Only r
transformToOnly []  = throw NotFound
transformToOnly _   = throw TooManyResults

execute :: (ToRow params, MonadIO m) => Query -> params -> DBT m NoContent
execute q params = do
  logQueryFormat q params
  PGT.execute q params
  pure NoContent

logQueryFormat :: (ToRow params, MonadIO m) => Query -> params -> DBT m ()
logQueryFormat q params = do
  msg <- PGT.formatQuery q params
  liftIO $ cyanMessage $ "[QUERY] " <> decodeUtf8 msg
  pure ()

runDB :: (MonadCatch m, MonadBaseControl IO m) => ConnectionPool -> DBT m a -> m (Either InternalError a)
runDB pool action = try $ withResource pool $ runDBTSerializable action
