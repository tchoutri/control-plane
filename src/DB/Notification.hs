{-# LANGUAGE OverloadedLists #-}
module DB.Notification
  ( Notification (..)
  , NotificationId (..)
  , NotificationStatus (..)
  , NotificationPayload (..)
  , NewStatusPayload (..)
  , NotificationStatusPayload (..)
  , readNotification
  , getNotificationById
  , insertNotification
  , getNotifications
  , updateNotification
  ) where

import Control.Exception (throw)
import Data.Aeson (FromJSON, ToJSON (toJSON), object)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (Entity (..), _select, insert, selectById, updateFieldsBy)
import Database.PostgreSQL.Entity.DBT (DBError (ConstraintError), QueryNature (Select), query_)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Transact (DBT)

import DB.Types (InternalError (..))

newtype NotificationId
  = NotificationId { getNotificationId :: UUID }
  deriving stock (Eq, Generic)
  deriving newtype (FromField, FromJSON, Show, ToField, ToJSON)

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
                 }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

instance Entity Notification where
  tableName = "notifications"
  primaryKey = "notification_id"
  fields = [ "notification_id"
           , "device"
           , "title"
           , "message"
           , "received_at"
           , "status"
           , "read_at"
           ]

data Notification'
  = Notification' { notificationId :: NotificationId
                  , device         :: Text
                  , title          :: Text
                  , message        :: Text
                  , receivedAt     :: UTCTime
                  , status'        :: NotificationStatusEnum
                  , readAt         :: Maybe UTCTime
                  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)

data NotificationStatusEnum = NotificationRead' | NotificationUnread' deriving (Eq, Generic, Show)

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
          _                              -> throw (InternalError $ ConstraintError $ "Incoherent status for status ('"<> show status' <>"') and readAt ('" <> show readAt <> "') for notification " <> show notificationId)
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
                        }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

newtype NewStatusPayload
  = NewStatusPayload { status :: NotificationStatusPayload }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data NotificationStatusPayload = SetAsRead | SetAsUnread deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

instance ToField NotificationStatusPayload where
  toField SetAsRead   = Escape "Read"
  toField SetAsUnread = Escape "Unread"

-- Request functions

readNotification :: NotificationId -> DBT IO ()
readNotification nid = do
  ts <- liftIO getCurrentTime
  void $ updateFieldsBy @Notification ["status", "read_at"] (primaryKey @Notification, nid) (NotificationRead', Just ts)

getNotifications :: DBT IO (Vector Notification)
getNotifications = query_ Select (_select @Notification)

getNotificationById :: NotificationId -> DBT IO Notification
getNotificationById notificationId = selectById @Notification (Only notificationId)

insertNotification :: Notification -> DBT IO ()
insertNotification notification = insert @Notification notification

updateNotification :: Notification -> DBT IO ()
updateNotification Notification{..} = void $ updateFieldsBy @Notification ["status", "read_at"] (primaryKey @Notification, notificationId) params
  where
    params = case status of
                NotificationRead ts -> (NotificationRead', Just ts)
                NotificationUnread  -> (NotificationUnread', Nothing)
