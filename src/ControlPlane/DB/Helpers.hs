module ControlPlane.DB.Helpers where

import Colourista.IO              (cyanMessage)
import Control.Exception          (try, throw)
import Data.Pool
import Data.Time
import Database.PostgreSQL.Simple as PG
import Servant (NoContent (..))

import ControlPlane.DB.Types

mkPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO ConnectionPool
mkPool connectInfo subPools timeout connections = 
  createPool (connect connectInfo) close subPools timeout connections

queryMany :: (ToRow params, FromRow result) => ConnectionPool -> Query -> params -> IO (Either InternalError [result])
queryMany pool q params =
  try . withResource pool $ \conn -> do
    logQueryFormat conn q params
    PG.query conn q params

queryOne :: (ToRow params, FromRow result) => ConnectionPool -> Query -> params -> IO (Either InternalError (Only result))
queryOne pool q params = do
  result <- try . withResource pool $ \conn -> do
    logQueryFormat conn q params
    PG.query conn q params
  pure $ transformToOnly result

transformToOnly :: Either InternalError [result] -> Either InternalError (Only result)
transformToOnly (Right [r]) = Right (Only r)
transformToOnly (Right [])  = Left NotFound
transformToOnly (Right _)   = Left TooManyResults
transformToOnly (Left err)  = Left err

execute :: (ToRow params) => ConnectionPool -> Query -> params -> IO (Either InternalError NoContent)
execute pool q params =
  try . withResource pool $ \conn ->
      withTransaction conn $ do
        logQueryFormat conn q params
        PG.execute conn q params
        pure NoContent

logQueryFormat :: (ToRow params) => Connection -> Query -> params -> IO ()
logQueryFormat conn q params = do
  msg <- formatQuery conn q params
  cyanMessage $ "[QUERY] " <> decodeUtf8 msg
  pure ()
