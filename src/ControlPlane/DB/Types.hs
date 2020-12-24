module ControlPlane.DB.Types
  ( ConnectionPool
  , ConnectInfo
  , createPool
  , close
  , connect
  , FromField
  , ToField
  ) where

import Data.Pool                            (Pool, createPool)
import Database.PostgreSQL.Simple           (ConnectInfo, Connection, close, connect)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField   (ToField)

type ConnectionPool = Pool Connection

