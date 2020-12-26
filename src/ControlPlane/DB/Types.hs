{-# LANGUAGE StrictData #-}
module ControlPlane.DB.Types
  ( ConnectInfo
  , ConnectionPool
  , FromField
  , InternalError (..)
  , ToField
  , close
  , connect
  , createPool
  ) where

import Data.UUID (UUID)
import Data.Aeson (ToJSON (..), object)
import Data.Pool                            (Pool, createPool)
import Database.PostgreSQL.Simple           (ConnectInfo, Connection, close, connect)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField   (ToField)

type ConnectionPool = Pool Connection

data InternalError
  = ConstraintFailure {-# UNPACK #-} Text
  | NotificationNotFound {-# UNPACK #-} UUID
  deriving stock (Show, Generic)

instance Exception InternalError

instance ToJSON InternalError where
  toJSON (ConstraintFailure msg)   = object [("error", "ConstraintFailure"), ("message", toJSON msg)]
  toJSON (NotificationNotFound _) = object [("error", "NotificationNotFound"), ("message", "Notification not found ")]
