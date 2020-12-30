{-# LANGUAGE QuasiQuotes #-}
module ControlPlane.DB.Notification where

import Control.Exception
import Data.Aeson
import Data.Time
import Data.UUID                            (UUID)
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.PostgreSQL.Simple.FromRow   (FromRow (..))
import Database.PostgreSQL.Simple.ToField   (Action (..), ToField (..))
import Database.PostgreSQL.Simple.ToRow     (ToRow (..))
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ
import Servant

import ControlPlane.DB.Helpers (execute, queryMany, queryOne)
import ControlPlane.DB.Types   (ConnectionPool, InternalError (..))

newtype NotificationId = NotificationId { getNotificationId :: UUID }
  deriving stock (Eq, Generic)
  deriving newtype (Show, FromHttpApiData, FromJSON, ToJSON, FromField, ToField)

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

data Notification'
  = Notification' { notificationId :: NotificationId
                  , device         :: Text
                  , title          :: Text
                  , message        :: Text
                  , receivedAt     :: UTCTime
                  , status'        :: NotificationStatusEnum
                  , readAt         :: Maybe UTCTime
                  } deriving stock (Eq, Show, Generic)
                    deriving anyclass (FromRow, ToRow)

data NotificationStatusEnum = NotificationRead' | NotificationUnread'
  deriving (Eq,Show, Generic)

instance ToField NotificationStatusEnum where
  toField NotificationRead'   = Escape "Read"
  toField NotificationUnread' = Escape "Unread"

instance FromField NotificationStatusEnum where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs -> 
        case parseNotificationStatusEnum bs of
          Just a  -> pure a
          Nothing -> returnError ConversionFailed f $ "Conversion error: Expected 'status' enum, got " <> decodeUtf8 bs <> " instead."

parseNotificationStatusEnum :: (Eq s, IsString s) => s -> Maybe NotificationStatusEnum
parseNotificationStatusEnum "Read"   = Just NotificationRead'
parseNotificationStatusEnum "Unread" = Just NotificationUnread'
parseNotificationStatusEnum _        = Nothing

instance FromRow Notification where
  fromRow = do
    Notification'{..} <- fromRow
    status <- case (status', readAt) of
          (NotificationRead', Just ts)   -> pure $ NotificationRead ts
          (NotificationUnread', Nothing) -> pure NotificationUnread
          _                              -> throw . ConstraintFailure $ "Incoherent status for status ('"<> show status' <>"') and readAt ('" <> show readAt <> "') for notification " <> show notificationId
    pure Notification{..}

instance ToRow Notification where
  toRow Notification{..} =
    let (status', readAt) = case status of
          NotificationRead ts -> (NotificationRead', Just ts)
          NotificationUnread  -> (NotificationUnread', Nothing)
    in toRow Notification'{..}

data NotificationPayload
  = NotificationPayload { device  :: Text
                        , title   :: Text
                        , message :: Text
                        } deriving stock (Eq, Show, Generic)
                          deriving anyclass (FromJSON)

newtype NewStatusPayload = NewStatusPayload { status :: NotificationStatusPayload }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data NotificationStatusPayload
  = SetAsRead | SetAsUnread
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON)

instance ToField NotificationStatusPayload where
  toField SetAsRead   = Escape "Read"
  toField SetAsUnread = Escape "Unread"

-- Request functions

getNotifications :: ConnectionPool -> IO (Either InternalError [Notification])
getNotifications pool = queryMany pool q ()
  where q = [sql| SELECT notification_id, device, title, message, received_at, status, read_at
                  FROM notifications |]

getNotificationById :: ConnectionPool -> NotificationId -> IO (Either InternalError (Only Notification))
getNotificationById pool notificationId = queryOne pool q (Only notificationId)
  where q = [sql| SELECT notification_id, device, title, message, received_at, status, read_at
                  FROM notifications
                  WHERE notification_id = ? |]

insertNotification :: ConnectionPool -> Notification -> IO (Either InternalError NoContent)
insertNotification pool notification = execute pool q notification
  where q = [sql| INSERT INTO notifications 
                  (notification_id, device, title, message, received_at, status, read_at)
                  VALUES (?,?,?,?,?,?,?) |]

updateNotification :: ConnectionPool -> Notification -> IO (Either InternalError NoContent)
updateNotification pool Notification{..} = execute pool q params
    where q = [sql| UPDATE notifications
                    SET (status, read_at) (?,?) 
                    WHERE notification_id = (?)
                    |]
          params = case status of
                      (NotificationRead ts) -> (NotificationRead', Just ts, notificationId)
                      NotificationUnread  -> (NotificationUnread', Nothing, notificationId)
