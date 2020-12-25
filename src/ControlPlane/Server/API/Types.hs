{-# LANGUAGE StrictData #-}
module ControlPlane.Server.API.Types where

import Data.Aeson

data InternalError
  = ConstraintFailure {-# UNPACK #-} Text
  | NotificationNotFound
  deriving stock (Show, Generic)

instance ToJSON InternalError where
  toJSON (ConstraintFailure msg) = object [("error", "ConstraintFailure"), ("message", toJSON msg)]
  toJSON NotificationNotFound    = object [("error", "NotificationNotFound"), ("message", "Notification not found ")]
