module ControlPlane.DB.Helpers where

import Colourista.IO              (cyanMessage)
import Control.Exception          (try)
import Data.Pool
import Data.Time
import Database.PostgreSQL.Simple as PG
import Servant (NoContent (..))

import ControlPlane.DB.Types

mkPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO ConnectionPool
mkPool connectInfo subPools timeout connections = 
  createPool (connect connectInfo) close subPools timeout connections

query :: (ToRow a, FromRow b) => ConnectionPool -> Query -> a -> IO (Either InternalError [b])
query pool q params = do
  try . withResource pool $ \conn -> do
    logQueryFormat conn q params
    PG.query conn q params

execute :: (ToRow a) => ConnectionPool -> Query -> a -> IO (Either InternalError NoContent)
execute pool q params = do
  try . withResource pool $ \conn ->
      withTransaction conn $ do
        logQueryFormat conn q params
        PG.execute conn q params
        pure NoContent

logQueryFormat :: (ToRow q) => Connection -> Query -> q -> IO ()
logQueryFormat conn q params = do
  msg <- formatQuery conn q params
  cyanMessage $ "[QUERY] " <> decodeUtf8 msg
  pure ()
