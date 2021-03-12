{-# LANGUAGE StrictData #-}
module DB.Types
  ( InternalError (..)
  ) where

import Data.Aeson (ToJSON (..), object)
import Database.PostgreSQL.Entity.DBT.Types

newtype InternalError
  = InternalError DBError
  deriving newtype (Eq, Show)

instance Exception InternalError

instance ToJSON InternalError where
  toJSON (InternalError dbe) = toJSON' dbe
    where
      toJSON' (ConstraintError msg)   = object [("error", "ConstraintFailure"), ("message", toJSON msg)]
      toJSON' InsertionError = object [("error", "InsertionError"), ("message","Could not insert incoming notification payload into database")]
      toJSON' NotFound = object [("error", "NotFound"), ("message", "Not found")]
      toJSON' TooManyResults = object [("error", "TooManyResults"), ("message", "Too many results")]
      toJSON' (DeserialisationError param) = object [("error", "DeserialisationError"), ("message", toJSON $ "Could not deserialise parameter" <> param <> " to the expected format")]
