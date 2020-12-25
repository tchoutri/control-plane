module ControlPlane.DB.Helpers where

import Colourista.IO              (cyanMessage)
import Control.Exception          (try)
import Data.Pool
import Data.Time
import Database.PostgreSQL.Simple as PG

import ControlPlane.DB.Types

mkPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO ConnectionPool
mkPool connectInfo subPools timeout connections = 
  createPool (connect connectInfo) close subPools timeout connections

query :: (ToRow a, FromRow b) => ConnectionPool -> Query -> a -> IO (Either IncoherentDataException [b])
query pool q params = do
  logQuery q
  try . withResource pool $ \conn -> do
    logQuery <$> PG.formatQuery conn q params
    PG.query conn q params

logQuery :: (Show a) => a -> IO ()
logQuery a = cyanMessage $ "[QUERY] " <> show a
