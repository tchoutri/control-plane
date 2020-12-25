{-# LANGUAGE StrictData #-}
module ControlPlane.DB.Types
  ( ConnectInfo
  , ConnectionPool
  , FromField
  , IncoherentDataException (..)
  , ToField
  , close
  , connect
  , createPool
  ) where

import Data.Pool                            (Pool, createPool)
import Database.PostgreSQL.Simple           (ConnectInfo, Connection, close, connect)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField   (ToField)

type ConnectionPool = Pool Connection

newtype IncoherentDataException
  = IDE { reason :: {-# UNPACK #-} Text
        } deriving stock (Show, Eq)

instance Exception IncoherentDataException 
