module ControlPlane.DB.Types
  ( ConnectionPool
  , ConnectInfo
  , createPool
  , close
  , connect
  ) where

import Data.Pool                            (Pool, createPool)
import Database.PostgreSQL.Simple           (Connection, ConnectInfo, close, connect)

type ConnectionPool = Pool Connection

