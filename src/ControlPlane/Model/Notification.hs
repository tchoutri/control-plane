{-| Used for functions related to the incoming notification payloads
-}
module ControlPlane.Model.Notification where

import Data.UUID.V4 (nextRandom)
import Data.Aeson (FromJSON)
import Data.Time (getCurrentTime)

import ControlPlane.DB.Notification

data NotificationPayload
  = NotificationPayload { device  :: Text
                        , title   :: Text
                        , message :: Text
                        } deriving stock (Eq, Show, Generic)
                          deriving anyclass (FromJSON)

mkNotification :: (MonadIO m) => NotificationPayload -> m Notification
mkNotification NotificationPayload {..} = do
  notificationId <- liftIO $ NotificationId <$> nextRandom
  let status = NotificationUnread
  receivedAt <- liftIO getCurrentTime
  pure Notification {..}
