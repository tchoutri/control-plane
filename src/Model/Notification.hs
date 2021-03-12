{-| Used for functions related to the incoming notification payloads
-}
module Model.Notification where

import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)

import DB.Notification (Notification (..), NotificationId (..), NotificationPayload (..), NotificationStatus (..))

mkNotification :: (MonadIO m) => NotificationPayload -> m Notification
mkNotification NotificationPayload {..} = do
  notificationId <- liftIO $ NotificationId <$> nextRandom
  let status = NotificationUnread
  receivedAt <- liftIO getCurrentTime
  pure Notification {..}
