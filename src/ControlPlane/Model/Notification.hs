module ControlPlane.Model.Notification where

import Data.Aeson
import Data.Time
import Data.UUID
import Servant

import ControlPlane.DB.Types

newtype NotificationId = NotificationId { getNotificationId :: UUID }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, FromJSON, ToJSON, FromField, ToField)

data NotificationStatus
  = NotificationRead UTCTime
  | NotificationUnread
  deriving stock (Eq, Show)

instance ToJSON NotificationStatus where
  toJSON (NotificationRead ts) = object [("status", "read"), ("readAt", toJSON ts)]
  toJSON NotificationUnread    = object [("status", "unread")]

data Notification
  = Notification { notificationId :: NotificationId
                 , device         :: Text
                 , title          :: Text
                 , message        :: Text
                 , status         :: NotificationStatus
                 , receivedAt     :: UTCTime
                 } deriving stock (Eq, Show, Generic)
                   deriving anyclass (ToJSON)
