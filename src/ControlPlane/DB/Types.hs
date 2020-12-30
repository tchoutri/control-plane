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

import Data.Aeson (ToJSON (..), object)
import Data.Pool                            (Pool, createPool)
import Database.PostgreSQL.Simple           (ConnectInfo, Connection, close, connect)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField   (ToField)

type ConnectionPool = Pool Connection

data InternalError
  = ConstraintFailure {-# UNPACK #-} Text
  | NotFound
  | TooManyResults
  | InsertionError
  deriving stock (Show, Generic)

instance Exception InternalError

instance ToJSON InternalError where
  toJSON (ConstraintFailure msg)   = object [("error", "ConstraintFailure"), ("message", toJSON msg)]
  toJSON InsertionError = object [("error", "InsertionError"), ("message","Could not insert incoming notification payload into database")]
  toJSON NotFound = object [("error", "NotFound"), ("message", "Not found")]
  toJSON TooManyResults = object [("error", "TooManyResults"), ("message", "Too many results")]
