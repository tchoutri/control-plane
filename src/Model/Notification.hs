{-| Used for functions related to the incoming notification payloads
-}
module Model.Notification where

import Data.UUID.V4 (nextRandom)
import Data.Time (getCurrentTime)

import DB.Notification

mkNotification :: (MonadIO m) => NotificationPayload -> m Notification
mkNotification NotificationPayload {..} = do
  notificationId <- liftIO $ NotificationId <$> nextRandom
  let status = NotificationUnread
  receivedAt <- liftIO getCurrentTime
  pure Notification {..}
