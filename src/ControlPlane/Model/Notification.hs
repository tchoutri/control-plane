module ControlPlane.Model.Notification where

import Data.UUID
import Data.Aeson
import Servant
import ControlPlane.DB.Types

newtype NotificationId = NotificationId { getNotificationId :: UUID }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, FromJSON, ToJSON, FromField, ToField)

data Notification
  = Notification { notificationId :: NotificationId
                 , device         :: Text
                 , title          :: Text
                 , message        :: Text
                 } deriving stock (Eq, Show, Generic)
                   deriving anyclass (FromJSON, ToJSON)
